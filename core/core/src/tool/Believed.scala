package grit.core.tool

import grit.core.visibility.Integrity

/** What a tool's results are believed at when a call's outcome does not say
  * ([[Outcome.Done.integrity]]).
  */
enum Believed derives CanEqual {

  /** At `integrity`. */
  case At(integrity: Integrity)

  /** As grit's own words, which say nothing grit did not write: at the level of the turn that
    * runs the call, so they never lower it.
    */
  case Own
}
