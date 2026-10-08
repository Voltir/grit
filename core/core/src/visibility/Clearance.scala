package grit.core.visibility

import grit.core.place.Place

/** A labelled row as reading it is decided. */
enum Item {

  /** Anything grit recorded in a conversation in `room`: a message, a tool result, a summary,
    * a closing, whatever the edge showed.
    */
  case InRoom(room: Place)

  /** A document, or a plugin's cache document, kept in `room`: the own room of the
    * transaction that wrote it ([[Clearance.own]]), never a room its writer names; `None` when
    * that transaction had no own room. Read as [[InRoom]] of its room is.
    */
  case Kept(room: Option[Place])
}

/** What one transaction reads of labelled rows, and the least label it writes at. It reads an
  * item when `everywhere` dominates its label, and anything recorded or kept in `own`'s room
  * when `own`'s label dominates it. An item in or kept in a direct message's room
  * ([[grit.core.place.Place.direct]]) is read only when that room is `own`'s, up to its label,
  * whatever `everywhere` dominates, unless it is `maintaining`, grit's own work on every row
  * ([[Maintenance]]).
  */
final case class Clearance private (
    everywhere: Label,
    own: Option[Clearance.Own],
    maintaining: Boolean
) {

  /** Whether it reads `item`, labelled `label`. */
  def reads(item: Item, label: Label): Boolean = {
    val room = item match {
      case Item.InRoom(r) => Some(r)
      case Item.Kept(r) => r
    }
    val elsewhere = maintaining || !room.exists(_.direct)
    (elsewhere && everywhere.dominates(label)) ||
    own.exists(o => room.contains(o.room) && o.label.dominates(label))
  }

  /** The least label what it writes is kept at: its own room's label, else `everywhere`. */
  def floor: Label = own.fold(everywhere)(_.label)
}

object Clearance {

  /** A reader's own room, by identity, and its label. */
  final case class Own(room: Place, label: Label)

  /** Reads what `label` dominates, wherever it was said, and writes at `label`. */
  def of(label: Label): Clearance = new Clearance(label, None, false)

  /** [[of]] `label`, reading direct messages' items as every other ([[Maintenance]]). */
  private[visibility] def maintaining(label: Label): Clearance = new Clearance(label, None, true)

  /** A reader in `room`, labelled `roomLabel`, for an asker cleared for `asker`: everything
    * recorded or kept in the room up to `roomLabel`; anything else, other rooms and documents
    * kept in none included, up to `roomLabel meet asker`.
    */
  def inRoom(room: Place, roomLabel: Label, asker: Label): Clearance =
    new Clearance(roomLabel.meet(asker), Some(Own(room, roomLabel)), false)
}
