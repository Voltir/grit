package grit.core.recipe

import scala.concurrent.duration.FiniteDuration

/** Where a pool's candidates come from, as of `t`, when the message the input is for was said:
  * what existed then in the reader's room ([[grit.core.store.Origin.room]]), never an entry of
  * a conversation its thread shows (its own, and its strand's); or the team's records. A `most`
  * under 1 keeps none.
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

  /** The documents best matching the message and its thread's tail ([[grit.core.recipe.Records.query]]) among
    * those current at t that the read reads ([[grit.core.document.DocumentSearch.nearby]]): the
    * best `most`, each `{label}: {its text's first line, at most [[Pool.RecordChars]]} ({the
    * day it was written})`.
    */
  case Records(most: Int)

  /** The section what it finds is shown under. */
  def section: Section = this match {
    case Channel(_, _) | Author(_, _) => Section.Nearby
    case Exchanges => Section.Exchanges
    case Records(_) => Section.Records
  }
}
