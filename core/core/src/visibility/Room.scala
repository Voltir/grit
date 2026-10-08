package grit.core.visibility

import grit.core.place.Place

/** Who may enter a room, as the edge hosting it reports it. */
enum RoomAccess {

  /** Anyone in its workspace may join (a public channel). */
  case Open

  /** Only whom its members invite (a private channel). */
  case Invited
}

/** A room as a labeller labels it: where it is, and its access when its edge has reported one. */
final case class Room(place: Place, access: Option[RoomAccess])
