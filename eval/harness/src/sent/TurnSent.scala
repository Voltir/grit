package grit.eval.harness.sent

import java.time.Instant

import grit.assembly.estimate.CharEstimate
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.{Message, Tokens}
import grit.core.place.Place
import grit.core.prompt.Fragment
import grit.core.provider.ModelRequest
import grit.core.speech.Outcome
import grit.core.stitch.Placed
import grit.core.store.{Entry, Payload, StoreError, Tx}
import grit.core.tool.{ToolSet, ToolSetId}
import grit.core.triage.Tags
import grit.dbos.engine.Reader
import grit.eval.harness.corpus.{Capture, Spent}
import grit.turn.{Turn, TurnJudge, TurnOffer, TurnRecord}

/** A recorded turn as its model was sent it, with the decisions that led there: its
  * `workflow` and `turn`, the `room` its conversation is at, the `epoch` it ran under, when
  * its first message was `heard`, and its `root`; the `prompt` fragments and the tool set
  * (`toolSet`, `tools`) its offer recorded; its model `calls`, rebuilt through this build's
  * code ([[TurnRecord.requests]]), the answer's request `picked` by its ledger row; every
  * ledger row of the workflow (`spent`), its reply or draft (`answer`) and `summary`, the
  * search `queries` assembly wrote and the `window` it recorded; the stitch placement of its
  * conversation's first message (`stitch`), triage's `tags` on its root, what became of a
  * heard root's draft (`speech`, `judged`), and the tool calls given up on (`givenUp`).
  */
final case class TurnSent(
    workflow: WorkflowId,
    turn: TurnRef,
    room: Place,
    epoch: String,
    heard: Option[Instant],
    root: TurnOffer.Root,
    prompt: Vector[Fragment],
    toolSet: ToolSetId,
    tools: Vector[ToolSet.Entry],
    calls: TurnRecord.Calls,
    picked: Option[Picked],
    spent: Vector[(EntryId, Spent)],
    answer: Option[Answer],
    summary: Option[String],
    queries: Vector[String],
    window: Option[Payload.Window],
    stitch: Option[Placed],
    tags: Option[Tags],
    speech: Option[Outcome],
    judged: Option[TurnJudge.Judgement],
    givenUp: Map[(Int, Int), TurnRecord.GivenUp]
) {

  /** The ledger row kept under `entry`, if any. */
  def row(entry: EntryId): Option[Spent] = spent.collectFirst { case (e, s) if e == entry => s }

  /** Each call of its reply, oldest first: its round, the entry its reply is kept under, and
    * its request as rebuilt; the answering call's as `picked` says, with tools on unless it
    * picked them off.
    */
  def requests: Vector[(Int, EntryId, ModelRequest)] =
    calls.looped.map(c => (c.round, c.reply, c.request)) ++
      calls.answered.map(a => (a.round, a.reply, if (picked.contains(Picked.Off)) a.off else a.on))

  /** Whether `request`, sent by the call whose reply is kept as `reply`, estimates
    * ([[CharEstimate]]) as that call's ledger row recorded; `None` when no row is kept.
    */
  def agrees(reply: EntryId, request: ModelRequest): Option[Boolean] =
    row(reply).map(_.estimated == CharEstimate.request(request))
}

/** A turn's answer: its reply, or the `draft` of a turn rooted on a heard message. */
final case class Answer(message: Message.Assistant, draft: Boolean)

/** Which request the answering call was sent, by the estimate its ledger row recorded
  * ([[CharEstimate]], the estimator the kit wires): `On` or `Off` when exactly that one's
  * estimate agrees, `Unsure` with both and the recorded estimate otherwise, or when the row
  * is gone. Under [[TurnRecord.Schemas.Recorded]] neither estimate is that of what was
  * sent, so the answer is always `Unsure`.
  */
enum Picked {
  case On
  case Off
  case Unsure(on: Tokens, off: Tokens, recorded: Option[Tokens])
}

object Picked {

  /** Which of `answered`'s requests estimates as `recorded`. */
  def of(answered: TurnRecord.Answered, recorded: Option[Tokens]): Picked = {
    val on = CharEstimate.request(answered.on)
    val off = CharEstimate.request(answered.off)
    (recorded.contains(on), recorded.contains(off)) match {
      case (true, false) => On
      case (false, true) => Off
      case _ => Unsure(on, off, recorded)
    }
  }
}

object TurnSent {

  /** Workflow `workflow`'s turn, read through `reader`. `Left` when it is not a turn's
    * workflow id, DBOS does not know it, its conversation is gone, a store cannot be read,
    * or [[TurnRecord.requests]] refuses it (its reason given).
    */
  def read(reader: Reader^, workflow: WorkflowId): Either[String, TurnSent] = {
    val id = WorkflowId.value(workflow)
    def read[A](what: String)(body: (Tx^) ?=> Either[StoreError, A]): Either[String, A] =
      reader.db.read(body).left.map(e => s"$what of $id unread: ${Capture.kind(e)}")
    for {
      turn <- TurnRef.fromWorkflowId(workflow).toRight(s"$id is not a turn's workflow id")
      recorded <- reader.workflow(workflow).toRight(s"DBOS does not know $id")
      steps <- reader.steps(workflow).left.map(e => s"steps of $id unread: ${Capture.kind(e)}")
      offered <- TurnRecord.offer(steps).flatMap(_.toRight(s"$id recorded no offer"))
      conversation <- read("conversation")(reader.conversations.get(turn.conversationId))
        .flatMap(_.toRight(s"the conversation of $id is gone"))
      mine <- read("entries")(reader.entries.ofTurn(turn))
      first = mine.minByOption(e => grit.core.id.EntrySeq.value(e.seq))
      prompt <- read("prompt")(reader.prompts.prompt(offered.prompt))
      set <- read("tool set")(reader.toolSets.get(offered.tools))
      calls <- reader.db
        .read((tx: Tx^) ?=>
          Right(
            TurnRecord.requests(
              turn,
              steps,
              TurnRecord.Reads(
                reader.entries,
                reader.principals,
                reader.documents,
                reader.prompts,
                reader.toolSets,
                reader.profiles
              )
            )
          )
        )
        .left
        .map(e => s"requests of $id unread: ${Capture.kind(e)}")
        .flatten
      rows <- read("ledger")(reader.ledger.of(workflow))
      opening <- read("conversation's first entry")(
        reader.entries
          .list(turn.conversationId)
          .map(_.minByOption(e => grit.core.id.EntrySeq.value(e.seq)))
      )
      stitch <- opening.fold[Either[String, Option[Placed]]](Right(None))(o =>
        read("stitch")(reader.stitches.placed(o.id))
      )
      tags <- first.fold[Either[String, Option[Tags]]](Right(None))(f =>
        read("tags")(reader.triage.of(Vector(f.id))).map(_.get(f.id))
      )
      speech <- TurnRecord.speech(steps)
      judged <- TurnRecord.judged(steps, offered.root)
    } yield {
      val spent = rows.map(r =>
        r.entry -> Spent(TurnRecord.role(turn, r.entry), r.model, r.usage, r.estimatedInput)
      )
      def kept(entry: EntryId): Option[Entry] = mine.find(_.id == entry)
      val answer = kept(turn.draftId)
        .collect { case Entry(_, _, _, _, _, Payload.Draft(m), _) => Answer(m, draft = true) }
        .orElse(kept(turn.replyId).collect {
          case Entry(_, _, _, _, _, Payload.Message(m: Message.Assistant), _) =>
            Answer(m, draft = false)
        })
      TurnSent(
        workflow,
        turn,
        conversation.origin.place,
        recorded.epoch,
        first.map(_.createdAt),
        offered.root,
        prompt.fragments,
        offered.tools,
        set.tools,
        calls,
        calls.answered.map(a =>
          Picked.of(a, spent.collectFirst { case (e, s) if e == a.reply => s.estimated })
        ),
        spent,
        answer,
        kept(grit.turn.TurnSummary.id(turn)).collect {
          case Entry(_, _, _, _, _, Payload.Summary(text), _) => text
        },
        mine.collect { case Entry(_, _, _, _, _, Payload.Query(text), _) => text },
        kept(Turn.windowId(turn)).collect { case Entry(_, _, _, _, _, w: Payload.Window, _) => w },
        stitch,
        tags,
        speech,
        judged,
        TurnRecord.givenUp(steps)
      )
    }
  }
}
