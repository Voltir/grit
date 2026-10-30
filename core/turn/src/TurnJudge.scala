package grit.turn

import grit.core.classify.{Ask, Classifier, StateJson}
import grit.core.context.Shown
import grit.core.document.{DocLabel, Document}
import grit.core.id.{EntryId, EntrySeq, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, Tokens, Usage}
import grit.core.period.Probability
import grit.core.provider.TokenEstimator
import grit.core.speech.Judged
import grit.core.stitch.{Stitching, Strand}
import grit.core.store.{Entry, Nearby, Payload, Speakers}

/** What judges an unprompted turn's draft before it is posted (ADR 0022): one classifier call,
  * two yes/no questions: whether what the draft states is in what the turn recalled or its
  * thread, and whether it answers the message the turn heard, not answered already. Their
  * weakest answer is its score ([[Judged.score]]).
  */
object TurnJudge {

  /** The id of the ledger row for the judge's call on `turn`'s draft. */
  def id(turn: TurnRef): EntryId = EntryId(s"judge:${WorkflowId.value(turn.workflowId)}")

  /** How much of the thread, from its end, the judge is shown. */
  val ThreadChars = 4_000

  /** How much of what the turn recalled, from its start, the judge is shown. */
  val RecalledChars = 60_000

  /** The judge's state: `{"message": ..., "thread": ..., "draft": ..., "recalled": ...}`,
    * the heard message's first [[ThreadChars]] (the thread leaves it out), the thread's last
    * [[ThreadChars]] (what came before the heard message, and every reply after it), the draft,
    * and what the turn recalled its first [[RecalledChars]].
    */
  final case class State(message: String, thread: String, draft: String, recalled: String)

  given StateJson[State] = StateJson.instance(s =>
    ujson.Obj(
      "message" -> s.message.take(ThreadChars),
      "thread" -> s.thread.takeRight(ThreadChars),
      "draft" -> s.draft,
      "recalled" -> s.recalled.take(RecalledChars)
    )
  )

  /** What became of judging a draft. */
  enum Judgement {

    /** The draft passed ([[said]]): nothing was asked. */
    case Passed

    /** The turn recalled nothing, as [[TurnJudge.state]] counts recall: nothing to ground a
      * draft in, so nothing was asked.
      */
    case NothingRecalled

    /** Scored, the request estimated at `estimated` input tokens. */
    case Scored(judged: Judged, estimated: Tokens)

    /** The classifier gave no answer, for `why`. */
    case Unjudged(why: String)
  }

  /** A judge call as its ledger row keeps it: the `model`, what it consumed, and its
    * request's `estimated` input tokens.
    */
  private[turn] final case class Call(model: String, usage: Usage, estimated: Tokens)

  private def yesNo(instructions: String) = Ask.yesNo[State](instructions, None, None)

  private val grounded = yesNo(
    "Read draft, recalled and thread. Is every fact draft states found in recalled or " +
      "thread? Anything neither contains is no, however true it may be."
  )

  // `worth` asks whether the draft answers the message: kept under its name, as `Judged` is.
  private val worth = yesNo(
    "Read message, thread and draft. Does draft answer what message, or the thread before " +
      "it, asks, and is it not " +
      "already answered in thread? A greeting, an aside, or a reply that only restates the " +
      "question is no."
  )

  /** A draft's text: `None` when it passes, its whole text, trimmed and lower-cased,
    * [[TurnPrompt.Pass]] or nothing.
    */
  def said(draft: Message.Assistant): Option[String] = {
    val text = draft.blocks.collect { case AssistantBlock.Text(t) => t }.mkString.trim
    Option.when(text.nonEmpty && text.toLowerCase != TurnPrompt.Pass)(text)
  }

  /** What `classifier` scores `state` at, in one call, its request estimated by
    * `estimator`; `Unjudged`, with why, when it is unavailable or its answer does not read;
    * `NothingRecalled`, asking nothing, when `state.recalled` is blank.
    */
  def judge(classifier: Classifier^, estimator: TokenEstimator, state: State): Judgement =
    if (state.recalled.trim.isEmpty) Judgement.NothingRecalled
    else
      classifier.ask(state, grounded.zip(worth)) match {
        case Right(answered) =>
          val (g, w) = answered.value
          Judgement.Scored(
            Judged(
              Probability.clamped(g),
              Probability.clamped(w),
              answered.model,
              answered.usage
            ),
            estimator.system(ujson.write(StateJson[State].json(state)))
          )
        case Left(error) => Judgement.Unjudged(error.said)
      }

  /** The judge's state for a draft `draft`, from `all` of its conversation's entries so far,
    * `showing` what the turn's request was shown ([[TurnShown.shown]]), `strand`
    * what its strand said, and `speakers` who wrote `all` and the strand: the message and the
    * thread are [[told]]'s, so the heard message is read once, under the message, and the
    * thread holds what came before it and every reply after it. Recalled is what the model was
    * shown ([[Shown]]) of each other
    * conversation's section but its strand's, then of each document but its pointer's line,
    * then of each piece of a file the window held, as the model was shown it, then of each
    * record among the window's entries whose earlier messages the thread does
    * not show whole (some cut whole or cut inside, or none in it), then each of the turn's own
    * tool results but a failed one, under its call as a person is shown it: a strand is context,
    * not recall.
    */
  private[turn] def state(
      all: Vector[Entry],
      showing: TurnShown.Showing,
      strand: Strand.Read,
      strandChars: Int,
      speakers: Speakers,
      draft: String
  ): State = {
    val Told(message, said, others, thread) = told(all, showing.mine, strand, strandChars, speakers)
    // A record stands for what was said before it, so it is recall only when the thread does
    // not show that whole: the thread holds none of it, or leaves a message out whole or cut
    // inside (its shown messages are then not all of them as said).
    def unshown(record: Entry): Boolean =
      thread.cut.nonEmpty || thread.shown != others || !said.headOption.exists(seq =>
        EntrySeq.value(seq) < EntrySeq.value(record.seq)
      )
    val sections = showing.nearby
      .filter {
        case Nearby.Along(_, _, _) => false
        case Nearby.Open(_, _, _) | Nearby.Closed(_, _, _) | Nearby.Asked(_, _, _) => true
      }
      .flatMap(Shown.section(_, showing.near, showing.named))
    val records = showing.window
      .filter(e =>
        e.payload match {
          case Payload.Closed(_, _, _) => unshown(e)
          case _ => false
        }
      )
      .flatMap(Shown.of(_, showing.named))
    // The judge reads nothing: a document's line saying how to read it is left out.
    val documents = showing.documents.map((d: Document, label: DocLabel) =>
      Shown.document(d.copy(pointer = None), label)
    )
    // The pieces of files the window held, each as the model was shown it, after the documents.
    val pieces = showing.pieces.map((k, label, more) => Shown.piece(k, label, more))
    // What the draft's own calls read, each under its call; a failed call grounds nothing.
    val read = showing.mine.collect {
      case Entry(_, _, _, _, _, Payload.Result(r, call), _, _) if !r.isError =>
        s"$call:\n${Shown.result(r).content}"
    }
    val recalled =
      (sections ++ documents ++ pieces ++ records).collect { case Message.User(text) => text } ++
        read
    State(message, thread.text, draft, recalled.mkString("\n\n"))
  }

  /** The heard message and the thread around it, as the judge and the needs gate
    * ([[TurnNeeds]]) are shown them: `message`, the turn's own first message (of `mine`) under
    * its speaker's name, empty when it has none; `thread`, the strand's excerpt within
    * `strandChars`, then every other message of `all` within [[ThreadChars]] altogether, one
    * per line under its speaker's name ("Assistant" for grit's); `said`, the seq of each
    * message of `all`, in order, and `others`, the lines of those but the heard one.
    */
  private[turn] final case class Told(
      message: String,
      said: Vector[EntrySeq],
      others: Vector[Stitching.Line],
      thread: Stitching.Fitted
  )

  /** [[Told]] of `all` of a conversation's entries so far, the turn's own `mine`, its strand's
    * `strand` and `speakers` who wrote them.
    */
  private[turn] def told(
      all: Vector[Entry],
      mine: Vector[Entry],
      strand: Strand.Read,
      strandChars: Int,
      speakers: Speakers
  ): Told = {
    val lines = all.flatMap { e =>
      def line(speaker: String, text: String) = Stitching.Line(speaker, text, e.createdAt)
      (e.payload match {
        case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
          val said = blocks.collect { case AssistantBlock.Text(t) => t }.mkString
          Option.when(said.nonEmpty)(line("Assistant", said))
        case Payload.Posted(text) => Option.when(text.nonEmpty)(line("Assistant", text))
        case p => p.said.map(line(speakers.of(e.id).getOrElse("Someone"), _))
      }).map(e.seq -> _)
    }
    // The heard message is the turn's own first message; it is read once, under `message`, so
    // the thread leaves it out. It counts as shown, as `message` shows it whole.
    val heard = mine
      .sortBy(e => EntrySeq.value(e.seq))
      .flatMap(e =>
        e.payload.said.map(said =>
          e.seq -> Stitching.Line(speakers.of(e.id).getOrElse("Someone"), said, e.createdAt)
        )
      )
      .headOption
    val others = lines.filterNot((seq, _) => heard.exists((h, _) => h == seq)).map(_._2)
    val thread = Stitching.fit(
      Stitching.excerpt(strand.opening, strand.said, speakers, strandChars),
      others,
      "\n",
      ThreadChars
    )
    Told(heard.fold("")((_, line) => line.shown), lines.map(_._1), others, thread)
  }
}
