package grit.turn

import grit.core.classify.{Ask, Classifier, ClassifierError, StateJson}
import grit.core.context.{Shown, Window}
import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.period.Probability
import grit.core.provider.TokenEstimator
import grit.core.speech.Judged
import grit.core.stitch.{Stitching, Strand}
import grit.core.store.{Entry, Nearby, Payload, Speakers}

/** What judges an unprompted turn's draft before it is posted (ADR 0022): one classifier call,
  * two yes/no questions about the draft against its thread and what the turn recalled. Their
  * weakest answer is its score ([[Judged.score]]).
  */
object TurnJudge {

  /** The id of the ledger row for the judge's call on `turn`'s draft. */
  def id(turn: TurnRef): EntryId = EntryId(s"judge:${WorkflowId.value(turn.workflowId)}")

  /** How much of the thread, from its end, the judge is shown. */
  val ThreadChars = 4_000

  /** How much of what the turn recalled, from its start, the judge is shown. */
  val RecalledChars = 60_000

  /** The judge's state: `{"thread": ..., "draft": ..., "recalled": ...}`, the thread's last
    * [[ThreadChars]], the draft, and what the turn recalled its first [[RecalledChars]].
    */
  final case class State(thread: String, draft: String, recalled: String)

  given StateJson[State] = StateJson.instance(s =>
    ujson.Obj(
      "thread" -> s.thread.takeRight(ThreadChars),
      "draft" -> s.draft,
      "recalled" -> s.recalled.take(RecalledChars)
    )
  )

  /** What became of judging a draft. */
  enum Judgement {

    /** The draft passed ([[said]]): nothing was asked. */
    case Passed

    /** The turn recalled no record and no other conversation: nothing to ground a draft in,
      * so nothing was asked.
      */
    case NothingRecalled

    /** Scored, the request estimated at `estimated` input tokens. */
    case Scored(judged: Judged, estimated: Tokens)

    /** The classifier gave no answer, for `why`. */
    case Unjudged(why: String)
  }

  private def yesNo(instructions: String) = Ask.yesNo[State](instructions, None, None)

  private val grounded = yesNo(
    "Read draft and recalled. Is what draft states found in recalled? Anything that recalled " +
      "does not contain is no, however true it may be."
  )

  private val worth = yesNo(
    "Read thread and draft. Would the people in thread want to be interrupted by draft now? " +
      "Recalling a decision, fact or earlier discussion they are asking about or reaching for " +
      "is yes. Small talk, or correcting a detail nobody relies on, is no."
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
        case Left(ClassifierError.Unavailable(why)) => Judgement.Unjudged(s"unavailable: $why")
        case Left(ClassifierError.Unreadable(why)) => Judgement.Unjudged(s"unreadable: $why")
      }

  /** The judge's state for a draft `draft`, from `all` of its conversation's entries so far,
    * `window` the turn's window over them, `near` the other conversations' entries its
    * sections name, `strand` what its strand said, and `speakers` who wrote each: the thread
    * is the strand's excerpt within `strandChars` ([[Stitching.excerpt]]), then each person's
    * message and each reply so far, the heard message and every reply after it included,
    * within [[ThreadChars]] altogether ([[Stitching.thread]]), one per line under its speaker's name ("Assistant" for grit's); recalled is each record the window showed and
    * each other conversation's section but its strand's, as the model was shown them
    * ([[Shown]]): a strand is context, not recall.
    */
  def state(
      all: Vector[Entry],
      window: Window,
      near: Vector[Entry],
      strand: Strand.Read,
      strandChars: Int,
      speakers: Speakers,
      draft: String
  ): State = {
    val thread = all.flatMap { e =>
      e.payload match {
        case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
          val said = blocks.collect { case AssistantBlock.Text(t) => t }.mkString
          Option.when(said.nonEmpty)(s"Assistant: $said")
        case Payload.Posted(text) => Option.when(text.nonEmpty)(s"Assistant: $text")
        case p => p.said.map(t => s"${speakers.of(e.id).getOrElse("Someone")}: $t")
      }
    }
    val bySeq = all.map(e => e.seq -> e).toMap
    val records = window.entries
      .flatMap(bySeq.get)
      .filter(e =>
        e.payload match {
          case Payload.Closed(_, _, _) => true
          case _ => false
        }
      )
    val recalled = records.flatMap(Shown.of(_, speakers)) ++
      window.nearby
        .filter {
          case Nearby.Along(_, _, _) => false
          case Nearby.Open(_, _, _) | Nearby.Closed(_, _, _) | Nearby.Asked(_, _, _) => true
        }
        .flatMap(Shown.section(_, near, speakers))
    State(
      Stitching.thread(
        Stitching.excerpt(strand.opening, strand.said, speakers, strandChars),
        thread.mkString("\n"),
        ThreadChars
      ),
      draft,
      recalled
        .collect { case Message.User(text) => text }
        .mkString("\n\n")
    )
  }
}
