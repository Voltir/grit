package grit.eval.harness.reply

import grit.core.context.Window

/** How a window rebuilt for a recorded turn stands to the one the turn recorded, by ids: the
  * entries of its own conversation, and its sections from elsewhere ([[Window.nearby]]).
  */
enum Drift {

  /** The same entries and the same sections, in the same order. */
  case Same

  /** Its own entries as recorded; its sections from elsewhere not. */
  case Nearby

  /** Its sections from elsewhere as recorded; its own entries not. */
  case Own

  /** Neither its own entries nor its sections as recorded. */
  case Both

  /** An entry the recorded window showed is no longer in the database (a purge took it), so no
    * rebuild can show it again.
    */
  case Gone
}

object Drift {

  /** `rebuilt` against `recorded`, `Gone` whenever `lost` (the recorded window names an entry
    * the database no longer holds). Notes are never compared.
    */
  def of(recorded: Window, rebuilt: Window, lost: Boolean): Drift =
    if (lost) Gone
    else
      (recorded.entries == rebuilt.entries, recorded.nearby == rebuilt.nearby) match {
        case (true, true) => Same
        case (true, false) => Nearby
        case (false, true) => Own
        case (false, false) => Both
      }
}
