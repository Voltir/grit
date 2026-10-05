package grit.eval.harness.reply

import grit.core.context.Window

/** How a window rebuilt for a recorded turn stands to the one the turn recorded, by ids: the
  * entries of its own conversation, and what it drew from elsewhere: its sections
  * ([[Window.nearby]]) and its documents ([[Window.documents]]).
  */
enum Drift {

  /** The same entries, the same sections and the same documents, in the same order. */
  case Same

  /** Its own entries as recorded; its sections from elsewhere or its documents not. */
  case Nearby

  /** Its sections from elsewhere and its documents as recorded; its own entries not. */
  case Own

  /** Neither its own entries nor what it drew from elsewhere as recorded. */
  case Both

  /** An entry or a document the recorded window showed is no longer in the database (a purge
    * or the document's retention took it), so no rebuild can show it again.
    */
  case Gone
}

object Drift {

  /** `rebuilt` against `recorded`, `Gone` whenever `lost` (the recorded window names an entry
    * or a document the database no longer holds). Notes are never compared.
    */
  def of(recorded: Window, rebuilt: Window, lost: Boolean): Drift =
    if (lost) Gone
    else
      (
        recorded.entries == rebuilt.entries,
        recorded.nearby == rebuilt.nearby && recorded.documents == rebuilt.documents
      ) match {
        case (true, true) => Same
        case (true, false) => Nearby
        case (false, true) => Own
        case (false, false) => Both
      }
}
