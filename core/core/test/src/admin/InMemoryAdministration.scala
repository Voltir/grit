package grit.core.admin

import java.time.Instant

import grit.core.identity.{Account, Principal, Standing, Vouched}
import grit.core.place.Place
import grit.core.store.{InMemoryVoucher, StoreError, Tx}
import grit.core.visibility.{GroupName, Recorded, RoomAccess, Visibility}
import grit.dbos.sql.TestTx

/** An in-memory [[Administration]] for tests, keeping [[AdministrationContract]], under
  * `visibility`, whose people are whom `voucher` links each account to. What it records of
  * rooms and groups is a [[Recorded]] it holds itself, read as a transaction's labels in force,
  * and its audit rows are [[kept]].
  */
final class InMemoryAdministration(visibility: Visibility, val voucher: InMemoryVoucher)
    extends Administration {

  // Each var holds an immutable value, written and read only on the test's own thread, through
  // the calls it makes and waits on.
  @caps.unsafe.untrackedCaptures
  private var recorded = Recorded.Empty

  /** Every audit row, oldest first. */
  @caps.unsafe.untrackedCaptures
  var kept = Vector.empty[AdministrationContract.Kept]

  /** Has `account` vouched a full member by the voucher's realm. */
  def member(account: Account): Unit = {
    val _ = voucher.vouch(Vouched(account, Standing.Full(None)))(using TestTx.fake)
  }

  /** A transaction whose labels in force are what this has recorded now, as one opened now
    * would read them.
    */
  def inForce: Tx = TestTx.inForce(visibility, recorded)

  /** Has `room`'s access reported as `access`, as its edge would. */
  def reported(room: Place, access: RoomAccess): Unit =
    recorded = keeping(recorded, room)(_.copy(access = Some(access)))

  def run(by: Account, room: Place, command: Command, at: Instant): Either[StoreError, Answer] = {
    val named = command match {
      case Command.Clearance(Some(of)) => whom(of)
      case _ => None
    }
    Administration.decide(whom(by), room, command, named)(using
      TestTx.inForce(visibility, recorded)
    ) match {
      case Left(answer) => Right(answer)
      case Right(allowed) =>
        val after = keep(recorded, allowed.change)
        if (after != recorded) {
          recorded = after
          kept = kept :+ AdministrationContract.Kept(by, at, allowed.change)
        }
        val person = allowed.change match {
          case Change.Clear(p, _) => whom(p)
          case Change.Remove(p, _) => whom(p)
          case Change.Relabel(_, _, _) | Change.Quiet(_, _) => None
        }
        Right(Administration.answer(allowed, person)(using TestTx.inForce(visibility, recorded)))
    }
  }

  /** Whom `account` is linked to now, as the voucher says. */
  private def whom(account: Account): Option[Principal] = Some(voucher.principal(account))

  /** `r` with `change` kept. */
  private def keep(r: Recorded, change: Change): Recorded = change match {
    case Change.Relabel(room, _, Change.To.Set(label)) =>
      keeping(r, room)(_.copy(label = Some(label)))
    case Change.Relabel(room, _, Change.To.Default(_)) => keeping(r, room)(_.copy(label = None))
    case Change.Quiet(room, on) => keeping(r, room)(_.copy(quiet = on))
    case Change.Clear(person, c) => adding(r, GroupName.own(c))(_ + person)
    case Change.Remove(person, c) => adding(r, GroupName.own(c))(_ - person)
  }

  /** `r` with `room`'s record changed by `f`; a room nothing is recorded of stays so when `f`
    * records nothing either.
    */
  private def keeping(r: Recorded, room: Place)(f: Recorded.Kept => Recorded.Kept): Recorded = {
    val nothing = Recorded.Kept(None, None, quiet = false)
    val now = f(r.rooms.getOrElse(room, nothing))
    if (!r.rooms.contains(room) && now == nothing) r
    else Recorded(r.rooms.updated(room, now), r.added)
  }

  /** `r` with `group`'s added members changed by `f`. */
  private def adding(r: Recorded, group: GroupName)(f: Set[Account] => Set[Account]): Recorded = {
    val now = f(r.added.getOrElse(group, Set.empty))
    Recorded(r.rooms, if (now.isEmpty) r.added.removed(group) else r.added.updated(group, now))
  }
}

object InMemoryAdministration {

  /** One under the shipped visibility, vouching no one: every command only reads, and any
    * change is refused ([[Refusal.NotVouched]]). For a suite whose edge runs no command.
    */
  def none(): InMemoryAdministration =
    new InMemoryAdministration(Visibility.Shipped, InMemoryVoucher.none())
}
