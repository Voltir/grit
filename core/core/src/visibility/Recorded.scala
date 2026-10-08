package grit.core.visibility

import grit.core.identity.Account
import grit.core.place.Place

/** What people and edges have recorded about rooms and groups beyond what the deployment
  * declares (ADR 0033), as one transaction read it when it opened: what [[grit.core.store.Tx]]
  * labels rooms and clears people by, beside the deployment's [[Visibility]].
  *
  * @param rooms
  *   each room anything is recorded of, by its place
  * @param added
  *   the accounts added to each group through grit
  */
final case class Recorded private[grit] (
    rooms: Map[Place, Recorded.Kept],
    added: Map[GroupName, Set[Account]]
)

object Recorded {

  /** What is recorded of a room: its access as its edge last reported it, the label set for it
    * through grit, and whether it is quiet (nothing is posted there unasked).
    */
  final case class Kept(access: Option[RoomAccess], label: Option[Label], quiet: Boolean)

  /** Nothing recorded: every room labelled as the deployment declares it, no one added to any
    * group.
    */
  val Empty: Recorded = new Recorded(Map.empty, Map.empty)
}
