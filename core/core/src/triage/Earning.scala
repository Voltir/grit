package grit.core.triage

import grit.core.classify.Answer
import grit.core.id.{EntryId, QuestionName}
import grit.core.period.Probability
import grit.core.store.{Entry, Payload}

/** Whether a period earns a written closing (ADR 0020): the one definition, never mirrored in
  * SQL.
  */
object Earning {

  /** The yes/no whose yes says a heard message is worth keeping: `durable`, the name every
    * question set triage has asked keeps it under.
    */
  val Durable: QuestionName =
    // A lowercase word, a declared name by QuestionName.of's rule, so no Left is taken;
    // EarningTests reads it.
    QuestionName.of("durable").fold(why => throw new IllegalStateException(why), identity)

  /** The probability that a heard message is worth keeping at or above which it earns its
    * period a written closing.
    */
  val DurableAt: Probability = Probability.clamped(0.5)

  /** Whether the period whose own entries are `entries` earns a written closing, `tags` being
    * what triage made of its heard ones. It does not only when it has entries, every one was
    * heard ([[Payload.Heard]]), and each was weighed ([[Tags.Weighed]]) worth keeping
    * ([[Durable]]) below [[DurableAt]]. So it fails open: a period grit was addressed in, or
    * with a heard message untagged, unanswered, or with no [[Durable]] yes/no among its
    * answers, earns one.
    */
  def earns(entries: Vector[Entry], tags: Map[EntryId, Tags]): Boolean =
    !(entries.nonEmpty && entries.forall(e =>
      e.payload match {
        case Payload.Heard(_) =>
          tags.get(e.id).exists {
            case Tags.Weighed(answers, _, _) =>
              answers.get(Durable).exists {
                case Answer.YesNo(yes) => !(Probability.clamped(yes) >= DurableAt)
                case Answer.Choice(_, _, _) | Answer.Score(_, _, _) => false
              }
            case Tags.Unanswered(_) => false
          }
        case _ => false
      }
    ))
}
