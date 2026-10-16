package grit.core.tool

import grit.core.visibility.{Cap, Integrity}

/** What a tool's results are believed at when a call's outcome does not say
  * ([[Outcome.Done.integrity]]). It covers a call at no service: one at a service is believed at
  * that service's declared output instead ([[grit.core.visibility.Visibility.outputOf]]).
  */
enum Believed derives CanEqual {

  /** At `integrity`. */
  case At(integrity: Integrity)

  /** As grit's own words, which say nothing grit did not write: at the level of the turn that
    * runs the call, so they never lower it.
    */
  case Own

  /** What it says in a turn at `level`. */
  def at(level: Integrity): Integrity = this match {
    case At(integrity) => integrity
    case Own => level
  }
}

object Believed {

  /** What a tool that declares nothing is believed at: [[Cap.Undeclared]]. */
  val Default: Believed = At(Cap.Undeclared)
}
