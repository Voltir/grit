package grit.core.edge

import java.time.Instant

import grit.core.identity.{Account, Realm, TestAccounts}
import grit.core.place.Place
import grit.core.visibility.RoomAccess
import grit.core.visibility.TestLabels.place

import utest.*

/** The contract every [[Joins]] keeps, run against the in-memory fake in core and the SQL joins
  * in grit.dbos: a room's membership ordered by when each join and leave happened, a join's
  * backfill skipped for an inviter no trusted realm vouches, and which rooms are forgotten.
  */
abstract class JoinsContract extends TestSuite {
  import JoinsContract.*

  /** Joins over a store that has seen no account and recorded nothing of any room. */
  protected def fresh(): Joins

  /** Has `account` vouched a full member by [[T1]], as its realm's attestation would. */
  protected def member(j: Joins, account: Account): Unit

  /** Has a person made `room` quiet, as a command would. */
  protected def decide(j: Joins, room: Place): Unit

  /** `room`'s access as recorded; `None` when none is. */
  protected def access(j: Joins, room: Place): Option[RoomAccess]

  private def ok[A](r: Either[?, A]): A =
    r.fold(e => throw new java.lang.AssertionError(s"the store failed: $e"), identity)

  val tests = Tests {
    test(
      "a join makes the bot a member, its backfill pending; a repeat while a member changes nothing"
    ) {
      val j = fresh()
      (
        ok(j.joined(general, RoomAccess.Open, None, t(1))),
        ok(j.joined(general, RoomAccess.Open, None, t(2))),
        ok(j.joined(general, RoomAccess.Open, None, t(0)))
      ) ==> (
        Membership.Member(t(1), Some(t(1))),
        Membership.Member(t(1), Some(t(1))),
        Membership.Member(t(1), Some(t(1)))
      )
    }

    test("a leave reported before the join it follows, made after it, leaves the bot gone") {
      val j = fresh()
      (
        ok(j.left(general, t(2))),
        ok(j.joined(general, RoomAccess.Open, None, t(1))),
        ok(j.members(Team))
      ) ==> (Membership.Gone, Membership.Gone, Vector.empty)
    }

    test("a leave made before the join it is reported after changes nothing") {
      val j = fresh()
      (
        ok(j.joined(general, RoomAccess.Open, None, t(2))),
        ok(j.left(general, t(1))),
        ok(j.members(Team))
      ) ==> (
        Membership.Member(t(2), Some(t(2))),
        Membership.Member(t(2), Some(t(2))),
        Vector(general -> Membership.Member(t(2), Some(t(2))))
      )
    }

    test("a join and a leave at the same instant leave the bot gone, whichever is reported first") {
      val j = fresh()
      ok(j.joined(general, RoomAccess.Open, None, t(1)))
      ok(j.left(ops, t(1)))
      (
        ok(j.left(general, t(1))),
        ok(j.joined(ops, RoomAccess.Invited, None, t(1)))
      ) ==> (Membership.Gone, Membership.Gone)
    }

    test("a join after a leave is a new join, its backfill pending again") {
      val j = fresh()
      ok(j.joined(general, RoomAccess.Open, Some(gus), t(1)))
      ok(j.left(general, t(2)))
      ok(j.joined(general, RoomAccess.Open, None, t(3))) ==> Membership.Member(t(3), Some(t(3)))
    }

    test(
      "a join invited by someone no trusted realm vouches a full member skips its backfill; by a member, or with no inviter named, it does not"
    ) {
      val j = fresh()
      member(j, mia)
      (
        ok(j.joined(general, RoomAccess.Open, Some(gus), t(1))),
        ok(j.joined(ops, RoomAccess.Invited, Some(mia), t(1))),
        ok(j.joined(trial, RoomAccess.Invited, None, t(1)))
      ) ==> (
        Membership.Member(t(1), None),
        Membership.Member(t(1), Some(t(1))),
        Membership.Member(t(1), Some(t(1)))
      )
    }

    test("a join records the room's access as reported, a join that changes nothing too") {
      val j = fresh()
      ok(j.joined(general, RoomAccess.Open, None, t(1)))
      val first = access(j, general)
      ok(j.joined(general, RoomAccess.Invited, None, t(2)))
      (first, access(j, general), access(j, ops)) ==>
        (Some(RoomAccess.Open), Some(RoomAccess.Invited), None)
    }

    test(
      "backfilled marks the backfill of a room's last join done, so it is no longer pending; one of an earlier join, or of a room never joined, changes nothing"
    ) {
      val j = fresh()
      ok(j.joined(general, RoomAccess.Open, None, t(1)))
      ok(j.joined(ops, RoomAccess.Invited, None, t(1)))
      ok(j.left(ops, t(2)))
      ok(j.joined(ops, RoomAccess.Invited, None, t(3)))
      (
        ok(j.backfilled(general, t(1))),
        ok(j.backfilled(ops, t(1))),
        ok(j.backfilled(trial, t(1))),
        ok(j.members(Team))
      ) ==> (
        (),
        (),
        (),
        Vector(
          general -> Membership.Member(t(1), None),
          ops -> Membership.Member(t(3), Some(t(3)))
        )
      )
    }

    test(
      "backfilledSince counts the rooms within a place whose latest join with its backfill done or skipped was made at or after then; none pending, earlier or elsewhere"
    ) {
      val j = fresh()
      ok(j.joined(general, RoomAccess.Open, None, t(5)))
      ok(j.backfilled(general, t(5)))
      ok(j.joined(ops, RoomAccess.Invited, Some(gus), t(6)))
      ok(j.joined(trial, RoomAccess.Invited, None, t(1)))
      ok(j.backfilled(trial, t(1)))
      ok(j.joined(lounge, RoomAccess.Open, None, t(7)))
      ok(j.joined(elsewhere, RoomAccess.Open, None, t(8)))
      ok(j.backfilled(elsewhere, t(8)))
      (ok(j.backfilledSince(Team, t(5))), ok(j.backfilledSince(Team, t(6)))) ==> (2, 1)
    }

    test("members are the rooms within a place the bot is a member of, and no others") {
      val j = fresh()
      ok(j.joined(general, RoomAccess.Open, None, t(1)))
      ok(j.joined(ops, RoomAccess.Invited, Some(gus), t(2)))
      ok(j.joined(trial, RoomAccess.Invited, None, t(1)))
      ok(j.left(trial, t(3)))
      ok(j.joined(elsewhere, RoomAccess.Open, None, t(1)))
      ok(j.members(Team)) ==> Vector(
        general -> Membership.Member(t(1), Some(t(1))),
        ops -> Membership.Member(t(2), None)
      )
    }

    test(
      "forget forgets a room left before then and not rejoined, so a join reported after is recorded; a member, one left since, and one quieted are kept"
    ) {
      val j = fresh()
      ok(j.joined(general, RoomAccess.Open, None, t(1)))
      ok(j.left(general, t(2)))
      ok(j.joined(ops, RoomAccess.Invited, None, t(1)))
      ok(j.left(ops, t(2)))
      decide(j, ops)
      ok(j.joined(trial, RoomAccess.Invited, None, t(1)))
      ok(j.left(trial, t(5)))
      ok(j.joined(lounge, RoomAccess.Open, None, t(1)))
      ok(j.left(elsewhere, t(2)))
      (
        ok(j.forget(Team, t(3))),
        access(j, general),
        access(j, ops),
        ok(j.joined(general, RoomAccess.Open, None, t(1))),
        ok(j.joined(ops, RoomAccess.Invited, None, t(1))),
        ok(j.joined(trial, RoomAccess.Invited, None, t(1)))
      ) ==> (
        1,
        None,
        Some(RoomAccess.Invited),
        Membership.Member(t(1), Some(t(1))),
        Membership.Gone,
        Membership.Gone
      )
    }

    test(
      "a room left is not forgotten as of its own leave: a join made before it is still ignored"
    ) {
      val j = fresh()
      ok(j.left(general, t(2)))
      (
        ok(j.forget(Team, t(2))),
        ok(j.joined(general, RoomAccess.Open, None, t(1)))
      ) ==> (0, Membership.Gone)
    }
  }
}

object JoinsContract {

  def t(seconds: Int): Instant = Instant.parse("2026-10-08T09:00:00Z").plusSeconds(seconds)

  /** The realm whose full members' invitations are backfilled. */
  val T1: Realm = Realm.of("slack", "T1").fold(e => throw new java.lang.AssertionError(e), identity)

  /** The workspace the rooms below are within, but [[elsewhere]]. */
  val Team: Place = place("slack:T1")

  val general: Place = place("slack:T1/C-general")
  val ops: Place = place("slack:T1/C-ops")
  val trial: Place = place("slack:T1/C-trial")
  val lounge: Place = place("slack:T1/C-lounge")

  /** A room of another workspace. */
  val elsewhere: Place = place("slack:T2/C-general")

  /** A full member of [[T1]], once a test vouches her. */
  val mia: Account = TestAccounts.account("slack:T1/U-mia")

  /** No realm vouches him a member. */
  val gus: Account = TestAccounts.account("slack:T1/U-gus")
}
