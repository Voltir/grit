package grit.eval.harness.sent

import java.time.Instant

import grit.assembly.estimate.CharEstimate
import grit.core.context.CarriedWindow
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
import grit.dbos.internal.Reader
import grit.eval.harness.capture.Spent
import grit.turn.{Turn, TurnJudge, TurnOffer, TurnRecord, TurnSummary}

/** A recorded turn as its model was sent it, with the decisions that led there: its
  * `workflow` and `turn`, the `room` its conversation is at, the `epoch` it ran under, when
  * its first message was `heard`, and its `root`; the `prompt` fragments and the tool set
  * (`toolSet`, `tools`) its offer recorded; its model `calls`, rebuilt through this build's
  * code ([[TurnRecord.requests]]), the answer's request `picked` by its ledger row; every
  * ledger row of the workflow (`spent`), each with the thinking its reply's kept usage reports,
  * its reply or draft (`answer`) and `summary`, the
  * search `queries` assembly wrote and the `window` it was shown, a carried window's parts
  * included ([[CarriedWindow.whole]]); the stitch placement of its
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

  /** `request`'s estimate ([[CharEstimate]]) over the input the provider counted for the call
    * whose reply is kept as `reply`, to two places: above 1 the estimate errs high. `None`
    * when no row is kept or its input is 0.
    */
  def share(reply: EntryId, request: ModelRequest): Option[BigDecimal] =
    row(reply)
      .map(r => Tokens.value(r.usage.input))
      .filter(_ > 0)
      .map(in =>
        (BigDecimal(Tokens.value(CharEstimate.request(request))) / in)
          .setScale(2, BigDecimal.RoundingMode.HALF_UP)
      )

  /** Each call of its reply, as [[requests]] gives them, then its summary's when it is kept,
    * measured against its rows under [[CharEstimate]] ([[MeasuredCall.of]]): a summary asked on
    * the answering call's prefix as the next of those calls, one asked alone apart, no window
    * before it.
    */
  def measured: Vector[MeasuredCall] = {
    val looped = requests.map((_, reply, request) => reply -> request)
    val id = TurnSummary.id(turn)
    val onPrefix = calls.summary.filter(s =>
      calls.answered.exists(a =>
        Vector(a.on, a.off).exists(c =>
          s.system == c.system && s.tools == c.tools && s.use == c.use &&
            s.messages.startsWith(c.messages)
        )
      )
    )
    val alone = calls.summary.filter(_ => onPrefix.isEmpty)
    MeasuredCall.of(looped ++ onPrefix.map(id -> _), row, calls.window, CharEstimate) ++
      MeasuredCall.of(alone.map(id -> _).toVector, row, Vector.empty, CharEstimate)
  }

  /** The tool results and tools of the request of its last call, measured against what the
    * model wrote after each and its answer ([[Referenced]]) under [[CharEstimate]]; `None`
    * when it made no call.
    */
  def referenced: Option[Referenced.Read] =
    requests.lastOption.map((_, _, request) =>
      Referenced.Read(
        Referenced.tools(request, answer.map(_.message), CharEstimate),
        Referenced.results(request, answer.map(_.message), CharEstimate)
      )
    )
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
      reader.all.read(body).left.map(e => s"$what of $id unread: ${e.kind}")
    for {
      turn <- TurnRef.fromWorkflowId(workflow).toRight(s"$id is not a turn's workflow id")
      recorded <- reader.workflow(workflow).toRight(s"DBOS does not know $id")
      steps <- reader.steps(workflow).left.map(e => s"steps of $id unread: ${e.kind}")
      offered <- TurnRecord.offer(steps).flatMap(_.toRight(s"$id recorded no offer"))
      conversation <- read("conversation")(reader.conversations.get(turn.conversationId))
        .flatMap(_.toRight(s"the conversation of $id is gone"))
      mine <- read("entries")(reader.entries.ofTurn(turn))
      first = mine.minByOption(e => grit.core.id.EntrySeq.value(e.seq))
      prompt <- read("prompt")(reader.prompts.prompt(offered.prompt))
      set <- read("tool set")(reader.toolSets.get(offered.tools))
      calls <- reader.all
        .read((tx: Tx^) ?=>
          Right(
            TurnRecord.requests(
              turn,
              steps,
              TurnRecord.Reads(
                reader.conversations,
                reader.entries,
                reader.principals,
                reader.documents,
                reader.pieces,
                reader.prompts,
                reader.toolSets,
                reader.profiles
              )
            )
          )
        )
        .left
        .map(e => s"requests of $id unread: ${e.kind}")
        .flatten
      rows <- read("ledger")(reader.ledger.of(workflow))
      all <- read("conversation's entries")(reader.entries.list(turn.conversationId))
      opening = all.minByOption(e => grit.core.id.EntrySeq.value(e.seq))
      stitch <- opening.fold[Either[String, Option[Placed]]](Right(None))(o =>
        read("stitch")(reader.stitches.placed(o.id))
      )
      tags <- first.fold[Either[String, Option[Tags]]](Right(None))(f =>
        read("tags")(reader.triage.of(Vector(f.id))).map(_.get(f.id))
      )
      speech <- TurnRecord.speech(steps)
      judged <- TurnRecord.judged(steps, offered.root)
    } yield {
      def kept(entry: EntryId): Option[Entry] = mine.find(_.id == entry)
      // The ledger keeps no thinking: the reply's own usage, kept with its entry, says it.
      def thinking(entry: EntryId): Option[Tokens] = kept(entry).flatMap(_.payload match {
        case Payload.Message(m: Message.Assistant) => m.usage.thinking
        case Payload.Draft(m) => m.usage.thinking
        case _ => None
      })
      val spent = rows.map(r =>
        r.entry -> Spent(
          TurnRecord.role(turn, r.entry),
          r.model,
          r.usage.copy(thinking = thinking(r.entry)),
          r.estimatedInput
        )
      )
      val answer = kept(turn.draftId)
        .collect { case Entry(_, _, _, _, _, Payload.Draft(m), _, _) => Answer(m, draft = true) }
        .orElse(kept(turn.replyId).collect {
          case Entry(_, _, _, _, _, Payload.Message(m: Message.Assistant), _, _) =>
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
          case Entry(_, _, _, _, _, Payload.Summary(text), _, _) => text
        },
        mine.collect { case Entry(_, _, _, _, _, Payload.Query(text), _, _) => text },
        CarriedWindow.whole(Turn.windowId(turn), all),
        stitch,
        tags,
        speech,
        judged,
        TurnRecord.givenUp(steps)
      )
    }
  }
}
