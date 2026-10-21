package grit.turn

import grit.core.classify.{Ask, StateJson}
import grit.core.period.Probability

/** Whether a turn's message needs anything recalled before its window is drawn (ADR 0040), the
  * `ask:needs` move: one yes/no about its message and the thread before it, asked of the turn's
  * classifier when a search could find anything. A turn searches unless the gate rejected its
  * message; a heard turn it rejected ends before its draft, as nothing recalled.
  */
object TurnNeeds {

  /** The probability of yes at or above which a turn searches: low, so a message is searched
    * for unless the classifier is nearly sure it needs nothing, as a "no" ends a heard turn.
    */
  val Pass: Probability = Probability.clamped(0.2)

  /** What the call is shown: `{"message": ..., "thread": ...}`, as the judge is shown them
    * ([[TurnJudge.State]]): the heard message under its speaker's name, its first
    * [[TurnJudge.ThreadChars]], and the thread around it, the strand's excerpt then each other
    * message, its last [[TurnJudge.ThreadChars]].
    */
  final case class State(message: String, thread: String)

  given StateJson[State] = StateJson.instance(s =>
    ujson.Obj(
      "message" -> s.message.take(TurnJudge.ThreadChars),
      "thread" -> s.thread.takeRight(TurnJudge.ThreadChars)
    )
  )

  /** `told` as the call is shown it. */
  private[turn] def state(told: TurnJudge.Told): State =
    State(told.message, told.thread.text)

  /** The one yes/no question, its answer read as the probability of yes. */
  val ask: Ask[State, Probability] =
    Ask
      .yesNo[State](
        "Does answering the message need facts, decisions or documents that are not already " +
          "in the thread? Yes when it asks about, or depends on, something said, decided or " +
          "written elsewhere or earlier: a person, a project, a date, a document, a number or " +
          "a past discussion. No when the thread alone answers it, or it is a greeting, " +
          "thanks, a reaction or an aside.",
        None,
        None
      )
      .map(Probability.clamped)

  /** Why a turn the gate rejected draws its window with no search: its window's note, after
    * "no query: ".
    */
  val Unneeded: String = "the message needs nothing recalled beyond its thread"

  /** Why the gate asked nothing of a turn whose window could find nothing by searching. */
  val NothingToSearch: String = "nothing to search"

  /** The roots of a turn that ends when the gate rejects its message: a heard message grit
    * drafts a reply to, unasked. A turn said to grit (`Addressed`, `ByName`) always replies.
    */
  type Unasked = TurnOffer.Root.Heard.type | TurnOffer.Root.Named.type

  /** What a turn does after the gate's verdict. */
  enum After {

    /** It searches, as it would with no gate: the gate passed its message, or did not judge it. */
    case Searches

    /** Said to grit and rejected: its window is drawn with no search, noted [[Unneeded]]. */
    case Unsearched

    /** Heard and rejected: it ends before its draft, nothing recalled. */
    case Ends(root: Unasked)
  }

  /** What a turn whose root is `root` does after the gate's `verdict`. */
  def after(verdict: TurnRelevance.Verdict, root: TurnOffer.Root): After =
    verdict match {
      case TurnRelevance.Verdict.Passed(_) | TurnRelevance.Verdict.Unjudged(_) => After.Searches
      case TurnRelevance.Verdict.Rejected(_) => unasked(root).fold(After.Unsearched)(After.Ends(_))
    }

  /** `root` as an [[Unasked]] one, a turn that ends unasked; `None` when said to grit. */
  def unasked(root: TurnOffer.Root): Option[Unasked] = root match {
    case TurnOffer.Root.Heard => Some(TurnOffer.Root.Heard)
    case TurnOffer.Root.Named => Some(TurnOffer.Root.Named)
    case TurnOffer.Root.Addressed | TurnOffer.Root.ByName => None
  }
}
