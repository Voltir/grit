package grit.core.edge

import java.time.Instant

import grit.core.identity.{Account, Standing, Vouched}
import grit.core.place.Place
import grit.core.store.{InMemoryVoucher, StoreError}
import grit.core.visibility.RoomAccess
import grit.dbos.sql.TestTx

/** An in-memory [[Joins]] for tests, keeping [[JoinsContract]]: each room's membership as
  * [[Joins.Kept]] orders it, its access as last reported ([[access]]), and whether a person
  * decided its label or quiet ([[decide]]); an inviter is whom `voucher` links their account to.
  */
final class InMemoryJoins(val voucher: InMemoryVoucher) extends Joins {

  // Each var holds an immutable value, written and read only on the test's own thread, through
  // the calls it makes and waits on.
  @caps.unsafe.untrackedCaptures
  private var kept = Map.empty[Place, Joins.Kept]

  @caps.unsafe.untrackedCaptures
  private var reported = Map.empty[Place, RoomAccess]

  @caps.unsafe.untrackedCaptures
  private var decided = Set.empty[Place]

  /** When set, every call fails as the database would. */
  @caps.unsafe.untrackedCaptures
  var down = false

  /** Has `account` vouched a full member by the voucher's realm. */
  def member(account: Account): Unit = {
    val _ = voucher.vouch(Vouched(account, Standing.Full(None)))(using TestTx.fake)
  }

  /** Has a person set `room`'s label or made it quiet, as a command would. */
  def decide(room: Place): Unit = decided = decided + room

  /** `room`'s access as last reported; `None` when it never was, or it was forgotten. */
  def access(room: Place): Option[RoomAccess] = reported.get(room)

  private def unavailable = Left(StoreError.DatabaseError("the database is down"))

  def joined(
      room: Place,
      access: RoomAccess,
      inviter: Option[Account],
      at: Instant
  ): Either[StoreError, Membership] =
    if (down) unavailable
    else {
      val skip = Joins.skips(inviter.map(a => Some(voucher.principal(a))))
      val now = kept.getOrElse(room, Joins.Never).join(at, skip)
      kept = kept.updated(room, now)
      reported = reported.updated(room, access)
      Right(now.membership)
    }

  def left(room: Place, at: Instant): Either[StoreError, Membership] =
    if (down) unavailable
    else {
      val now = kept.getOrElse(room, Joins.Never).leave(at)
      kept = kept.updated(room, now)
      Right(now.membership)
    }

  def backfilled(room: Place, join: Instant): Either[StoreError, Unit] =
    if (down) unavailable
    else {
      kept.get(room).foreach(k => kept = kept.updated(room, k.backfilledAt(join)))
      Right(())
    }

  def backfilledSince(under: Place, since: Instant): Either[StoreError, Int] =
    if (down) unavailable
    else Right(kept.count((room, k) => room.within(under) && k.backfilledSince(since)))

  def members(under: Place): Either[StoreError, Vector[(Place, Membership.Member)]] =
    if (down) unavailable
    else
      Right(
        kept.toVector
          .filter((room, _) => room.within(under))
          .collect { case (room, k) =>
            k.membership match {
              case m: Membership.Member => Some(room -> m)
              case Membership.Gone => None
            }
          }
          .flatten
          .sortBy(_._1.written)
      )

  def forget(under: Place, before: Instant): Either[StoreError, Int] =
    if (down) unavailable
    else {
      val gone = kept.collect {
        case (room, k) if room.within(under) && k.forgotten(before, decided.contains(room)) => room
      }.toSet
      kept = kept -- gone
      reported = reported -- gone
      Right(gone.size)
    }
}

object InMemoryJoins {

  /** Joins whose voucher vouches no one: every join named an inviter skips its backfill. For a
    * suite whose edge records no joins, or does not care whose they are.
    */
  def none(): InMemoryJoins = new InMemoryJoins(InMemoryVoucher.none())
}
