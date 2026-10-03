package grit.core.recipe

import scala.concurrent.duration.FiniteDuration

/** Where a pool's candidates come from: what existed in the reader's room
  * ([[grit.core.store.Origin.room]]) when the message the input is for was said, at `t`; never
  * an entry of a conversation its thread shows (its own, and its strand's). A `most` under 1
  * keeps none.
  */
enum Source {

  /** Anyone's messages, grit's replies and posts included, said in [t − `within`, t): the
    * latest `most`.
    */
  case Channel(within: FiniteDuration, most: Int)

  /** The message's author's own messages said in [t − `within`, t): the latest `most`. */
  case Author(within: FiniteDuration, most: Int)

  /** The exchanges stitching offered its conversation's first message, as its kept placement
    * shows them, in the order offered; none when it was not placed.
    */
  case Exchanges

  /** The section what it finds is shown under. */
  def section: Section = this match {
    case Channel(_, _) | Author(_, _) => Section.Nearby
    case Exchanges => Section.Exchanges
  }
}
