package grit.core.visibility

import grit.core.identity.Account
import grit.core.place.Place

/** What the in-memory stores record of rooms and groups for tests, one [[Recorded]] shared by
  * every fake given it, as the SQL stores share `grit.rooms` and `grit.group_members`: a change
  * one fake makes is what every other reads as the labels in force from then on.
  */
final class InMemoryRecorded {

  // An immutable value, written and read only on the test's own thread, through the calls it
  // makes and waits on.
  @caps.unsafe.untrackedCaptures
  private var held = Recorded.Empty

  /** What is recorded now, as a transaction opened now reads it. */
  def now: Recorded = held

  /** Changes `room`'s record by `f`; whether that changed anything. A room nothing is recorded
    * of stays so when `f` records nothing either.
    */
  def room(room: Place)(f: Recorded.Kept => Recorded.Kept): Boolean = {
    val nothing = Recorded.Kept(None, None, quiet = false)
    val was = held.rooms.get(room)
    val now = f(was.getOrElse(nothing))
    if (was.contains(now) || (was.isEmpty && now == nothing)) false
    else {
      held = Recorded(held.rooms.updated(room, now), held.added)
      true
    }
  }

  /** Changes the accounts added to `group` through grit by `f`; whether that changed any. */
  def group(group: GroupName)(f: Set[Account] => Set[Account]): Boolean = {
    val was = held.added.getOrElse(group, Set.empty)
    val now = f(was)
    if (now == was) false
    else {
      held = Recorded(
        held.rooms,
        if (now.isEmpty) held.added.removed(group) else held.added.updated(group, now)
      )
      true
    }
  }

  /** Forgets everything recorded of `room`, as deleting its row does. */
  def forget(room: Place): Unit =
    held = Recorded(held.rooms.removed(room), held.added)
}
