package grit.core.triage

import grit.core.id.EntryId
import grit.core.period.Probability
import grit.core.store.{Entry, Payload}

/** Whether a period earns a written closing (ADR 0020): the one definition, never mirrored in
  * SQL.
  */
object Earning {

  /** The probability that a heard message is worth keeping at or above which it earns its
    * period a written closing.
    */
  val DurableAt: Probability = Probability.clamped(0.5)

  /** Whether the period whose own entries are `entries` earns a written closing, `tags` being
    * what triage made of its heard ones. It does not only when it has entries, every one was
    * heard ([[Payload.Heard]]), and each was weighed ([[Tags.Weighed]]) worth keeping below
    * [[DurableAt]]. So it fails open: a period grit was addressed in, or with a heard message
    * untagged or unanswered, earns one.
    */
  def earns(entries: Vector[Entry], tags: Map[EntryId, Tags]): Boolean =
    !(entries.nonEmpty && entries.forall(e =>
      e.payload match {
        case Payload.Heard(_) =>
          tags.get(e.id).exists {
            case Tags.Weighed(_, _, _, durable, _, _, _) => !(durable >= DurableAt)
            case Tags.Unanswered(_) => false
          }
        case _ => false
      }
    ))
}
