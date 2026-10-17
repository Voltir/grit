package grit.dbos.engine

import grit.core.id.{TriageRef, TurnRef}
import grit.core.stitch.Opening

/** What recording a message starts, none of it started yet, in the order the engine starts
  * it: the placement of `opening` when the message opens a thread, then the rest.
  */
enum Starts {

  /** A message heard where grit listens: its triage; `None` when nothing new was heard, the
    * message recorded before as one said to grit, or refused by its room.
    */
  case Heard(opening: Option[Opening], triage: Option[TriageRef])

  /** A message said to grit: its turn. */
  case Said(opening: Option[Opening], turn: TurnRef)
}
