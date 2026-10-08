package grit.core.admin

import java.time.Instant

import grit.core.identity.{Account, Principal, Standing, Vouched}
import grit.core.place.Place
import grit.core.store.{InMemoryVoucher, StoreError, Tx}
import grit.core.visibility.{GroupName, InMemoryRecorded, RoomAccess, Visibility}
import grit.dbos.sql.TestTx

/** An in-memory [[Administration]] for tests, keeping [[AdministrationContract]], under
  * `visibility`, whose people are whom `voucher` links each account to. What it records of
  * rooms and groups it keeps in `voucher`'s [[InMemoryRecorded]], read as a transaction's
  * labels in force, and its audit rows are [[kept]].
  */
final class InMemoryAdministration(visibility: Visibility, val voucher: InMemoryVoucher)
    extends Administration {

  private val records: InMemoryRecorded = voucher.records

  /** Every audit row, oldest first. */
  @caps.unsafe.untrackedCaptures
  var kept = Vector.empty[AdministrationContract.Kept]

  /** Has `account` vouched a full member by the voucher's realm. */
  def member(account: Account): Unit = {
    val _ = voucher.vouch(Vouched(account, Standing.Full(None)))(using TestTx.fake)
  }

  /** A transaction whose labels in force are what is recorded now, as one opened now would
    * read them.
    */
  def inForce: Tx = TestTx.inForce(visibility, records.now)

  /** Has `room`'s access reported as `access`, as its edge would. */
  def reported(room: Place, access: RoomAccess): Unit = {
    val _ = records.room(room)(_.copy(access = Some(access)))
  }

  def run(by: Account, room: Place, command: Command, at: Instant): Either[StoreError, Answer] = {
    val named = command match {
      case Command.Clearance(Some(of)) => whom(of)
      case _ => None
    }
    Administration.decide(whom(by), room, command, named)(using inForce) match {
      case Left(answer) => Right(answer)
      case Right(allowed) =>
        if (keep(allowed.change))
          kept = kept :+ AdministrationContract.Kept(by, at, allowed.change)
        val person = allowed.change match {
          case Change.Clear(p, _) => whom(p)
          case Change.Remove(p, _) => whom(p)
          case Change.Relabel(_, _, _) | Change.Quiet(_, _) => None
        }
        Right(Administration.answer(allowed, person)(using inForce))
    }
  }

  /** Whom `account` is linked to now, as the voucher says. */
  private def whom(account: Account): Option[Principal] = Some(voucher.principal(account))

  /** Keeps `change`; whether anything recorded changed (a repeat changes nothing). */
  private def keep(change: Change): Boolean = change match {
    case Change.Relabel(room, _, Change.To.Set(label)) =>
      records.room(room)(_.copy(label = Some(label)))
    case Change.Relabel(room, _, Change.To.Default(_)) => records.room(room)(_.copy(label = None))
    case Change.Quiet(room, on) => records.room(room)(_.copy(quiet = on))
    case Change.Clear(person, c) => records.group(GroupName.own(c))(_ + person)
    case Change.Remove(person, c) => records.group(GroupName.own(c))(_ - person)
  }
}

object InMemoryAdministration {

  /** One under the shipped visibility, vouching no one: every command only reads, and any
    * change is refused ([[Refusal.NotVouched]]). For a suite whose edge runs no command.
    */
  def none(): InMemoryAdministration =
    new InMemoryAdministration(Visibility.Shipped, InMemoryVoucher.none())
}
