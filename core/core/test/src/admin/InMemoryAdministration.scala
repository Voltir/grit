package grit.core.admin

import java.time.Instant

import grit.core.id.TurnRef
import grit.core.identity.{Account, Principal, Standing, Vouched}
import grit.core.place.Place
import grit.core.store.{InMemoryVoucher, Origin, StoreError, Tx}
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
  // Replaced whole by the fake's own methods, which the test calls on its one thread.
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

  def run(by: Account, room: Place, command: Command, at: Instant): Either[StoreError, Answer] =
    within(by, room, command, at, None)

  def runFor(turn: TurnRef, command: Command, at: Instant): Either[StoreError, Answer] =
    turns.get(turn) match {
      case Some((origin, first)) =>
        Administration.askedThrough(origin, first) match {
          case Some(by) => within(by, origin.room, command, at, Some(turn))
          case None => Right(Answer.Refused(Refusal.NotVouched))
        }
      case None => Right(Answer.Refused(Refusal.NotVouched))
    }

  /** The turns begun, each with its conversation's origin and the account that wrote its
    * first entry, if one did.
    */
  // Replaced whole by the fake's own methods, which the test calls on its one thread.
  @caps.unsafe.untrackedCaptures
  private var turns = Map.empty[TurnRef, (Origin, Option[Account])]

  /** Has `turn` begun in `origin`'s conversation, its first entry written by `first`, or
    * holding none for `None`.
    */
  def began(turn: TurnRef, origin: Origin, first: Option[Account]): Unit =
    turns = turns.updated(turn, (origin, first))

  /** [[run]]'s body, its audit row naming `turn`. */
  private def within(
      by: Account,
      room: Place,
      command: Command,
      at: Instant,
      turn: Option[TurnRef]
  ): Either[StoreError, Answer] = {
    val named = command match {
      case Command.Clearance(Some(of)) => whom(of)
      case _ => None
    }
    Administration.decide(whom(by), room, command, named)(using inForce) match {
      case Left(answer) => Right(answer)
      case Right(allowed) =>
        if (keep(allowed.change))
          kept = kept :+ AdministrationContract.Kept(by, at, allowed.change, turn.map(_.workflowId))
        val person = allowed.change match {
          case Change.Clear(p, _) => whom(p)
          case Change.Remove(p, _) => whom(p)
          case Change.Relabel(_, _, _) | Change.Quiet(_, _) | Change.Baseline(_, _, _) => None
        }
        Right(Administration.answer(allowed, person, room)(using inForce))
    }
  }

  /** Whom `account` is linked to now, as the voucher says; `None` for one never seen. */
  private def whom(account: Account): Option[Principal] = voucher.known(account)

  /** Keeps `change`; whether anything recorded changed (a repeat changes nothing). */
  private def keep(change: Change): Boolean = change match {
    case Change.Relabel(room, _, Change.To.Set(label)) =>
      records.room(room)(_.copy(label = Some(label)))
    case Change.Relabel(room, _, Change.To.Default(_)) => records.room(room)(_.copy(label = None))
    case Change.Quiet(room, on) => records.room(room)(_.copy(quiet = on))
    case Change.Baseline(room, _, to) => records.room(room)(_.copy(baseline = to))
    case Change.Clear(person, c) =>
      // A person cleared is kept as an account, as SQL enrols them, seen from then on.
      voucher.saw(person)
      records.group(GroupName.own(c))(_ + person)
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
