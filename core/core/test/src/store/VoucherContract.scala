package grit.core.store

import scala.concurrent.duration.*

import grit.core.id.PrincipalId
import grit.core.identity.{Account, Domain, Email, Realm, Standing, TestAccounts, Vouched}
import grit.core.visibility.{
  Compartments,
  Grant,
  Group,
  Label,
  Level,
  RoomLabels,
  TestLabels,
  Visibility
}

import utest.*

/** The contract every [[Voucher]] keeps, run against the in-memory fake in core and the SQL
  * voucher in grit.dbos: what each answer reports, and how long ago each account was answered
  * for.
  */
abstract class VoucherContract extends TestSuite {
  import VoucherContract.*

  /** A voucher of [[T1]] and [[T2]], claiming [[Claimed]], clearing people as [[Cleared]]
    * does, over a store that has seen no account.
    */
  protected def fresh(): Voucher

  /** A voucher of no realms, as the kit builds for an edge whose deployment trusts it for none,
    * over a store that has seen no account.
    */
  protected def none(): Voucher

  /** Has the store `voucher` writes to see `account`, as a first message through it would. */
  protected def saw(voucher: Voucher, account: Account): Unit

  /** Arranges `account`'s last answer as given `ago` before now. */
  protected def aged(voucher: Voucher, account: Account, ago: FiniteDuration): Unit

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  private def vouch(v: Voucher, account: Account, standing: Standing): Vector[Linking] =
    transaction(v.vouch(Vouched(account, standing)))
      .fold(e => throw new java.lang.AssertionError(s"vouching: $e"), identity)

  private def full(address: String): Standing =
    Standing.Full(
      Some(Email.of(address).fold(e => throw new java.lang.AssertionError(e), identity))
    )

  /** The people `said` links accounts to, in order. */
  private def linkedTo(said: Vector[Linking]): Vector[PrincipalId] =
    said.collect { case Linking.Linked(_, to, _, _) => to }

  val tests = Tests {
    test("an account no realm of the voucher holds is outside, and nothing is kept of it") {
      val v = fresh()
      val a = TestAccounts.account("slack:T9/U-outside")
      (vouch(v, a, Standing.Full(None)), transaction(v.lastWord(a))) ==>
        (Vector(Linking.Outside(a)), Right(LastWord(a, None)))
    }

    test(
      "a voucher of no realms says every account is outside, a claimed member's included, and keeps no word of any"
    ) {
      val v = none()
      val a = TestAccounts.account("slack:T1/U-unattested")
      (vouch(v, a, full("unattested@example.com")), transaction(v.lastWord(a))) ==>
        (Vector(Linking.Outside(a)), Right(LastWord(a, None)))
    }

    test(
      "a first answer that an account is a full member makes it one, cleared as its realm's members are; the same answer again changes nothing"
    ) {
      val v = fresh()
      val a = TestAccounts.account("slack:T1/U-member")
      (vouch(v, a, Standing.Full(None)), vouch(v, a, Standing.Full(None))) ==> (
        Vector(Linking.Standing(a, member = true, Label.Public, Internal)),
        Vector()
      )
    }

    test(
      "two accounts attested one claimed email are one person: the second is linked to the first's, cleared for all that person's accounts are, and falls back to its own clearance when it leaves"
    ) {
      val v = fresh()
      val a = Named
      val b = TestAccounts.account("slack:T2/U-one-b")
      val first = vouch(v, a, full("one@example.com"))
      val second = vouch(v, b, full("one@example.com"))
      val left = vouch(v, b, Standing.Outside)
      val person = linkedTo(first)
      (first.toSet, second.toSet, left.toSet, linkedTo(second) == person && person.size == 1) ==> (
        person.map(p => Linking.Linked(a, p, Restricted, Restricted)).toSet +
          Linking.Standing(a, member = true, Restricted, Restricted),
        person.map(p => Linking.Linked(b, p, Label.Public, Restricted)).toSet +
          Linking.Standing(b, member = true, Label.Public, Restricted),
        person.map(p => Linking.Unlinked(b, p, Restricted, Label.Public)).toSet +
          Linking.Standing(b, member = false, Restricted, Label.Public),
        true
      )
    }

    test("an email in no claimed domain is not kept, and is said to be unclaimed") {
      val v = fresh()
      val a = TestAccounts.account("slack:T1/U-unclaimed")
      vouch(v, a, full("unclaimed@elsewhere.example")).toSet ==> Set(
        Linking.Standing(a, member = true, Label.Public, Internal),
        Linking.Unclaimed(a)
      )
    }

    test(
      "a member whose email moves to an unclaimed domain returns home, still a member, and its new email is said to be unclaimed"
    ) {
      val v = fresh()
      val a = TestAccounts.account("slack:T1/U-unclaiming")
      val person = linkedTo(vouch(v, a, full("before@example.com")))
      (person.size, vouch(v, a, full("after@elsewhere.example")).toSet) ==> (
        1,
        person.map(p => Linking.Unlinked(a, p, Internal, Internal)).toSet + Linking.Unclaimed(a)
      )
    }

    test("a full member said to be outside returns home and is a full member no longer") {
      val v = fresh()
      val a = TestAccounts.account("slack:T1/U-leaver")
      val person = linkedTo(vouch(v, a, full("leaver@example.com")))
      (person.size, vouch(v, a, Standing.Outside).toSet) ==> (
        1,
        person.map(p => Linking.Unlinked(a, p, Internal, Label.Public)).toSet +
          Linking.Standing(a, member = false, Internal, Label.Public)
      )
    }

    test("a changed email moves the account from the old email's person to the new one's") {
      val v = fresh()
      val a = TestAccounts.account("slack:T1/U-mover")
      val old = linkedTo(vouch(v, a, full("old@example.com")))
      val moved = vouch(v, a, full("new@example.com"))
      val now = linkedTo(moved)
      (moved.toSet, old.size, now.size, old != now) ==> (
        (old.map(p => Linking.Unlinked(a, p, Internal, Internal)) ++
          now.map(p => Linking.Linked(a, p, Internal, Internal))).toSet,
        1,
        1,
        true
      )
    }

    test(
      "an account has no last word until its realm answers for it, and then has one as old as that answer"
    ) {
      val v = fresh()
      val a = TestAccounts.account("slack:T1/U-aging")
      saw(v, a)
      val never = transaction(v.lastWord(a))
      val _ = vouch(v, a, Standing.Full(None))
      val just = transaction(v.lastWord(a)).map(_.ago.map(_ < 1.minute))
      aged(v, a, 5.minutes)
      val later = transaction(v.lastWord(a)).map(_.ago.map(d => d >= 5.minutes && d < 6.minutes))
      (never, just, later) ==> (Right(LastWord(a, None)), Right(Some(true)), Right(Some(true)))
    }

    test(
      "a realm's last words are of every account of it seen, answered for or not, and of no other realm's"
    ) {
      val v = fresh()
      val seen = TestAccounts.account("slack:T1/U-seen")
      val answered = TestAccounts.account("slack:T1/U-answered")
      val other = TestAccounts.account("slack:T2/U-other")
      saw(v, seen)
      val _ = vouch(v, answered, Standing.Full(None))
      val _ = vouch(v, other, Standing.Full(None))
      transaction(v.lastWords(T1)).map(
        _.map(w => (Account.written(w.account), w.ago.isDefined)).sorted
      ) ==> Right(Vector(("slack:T1/U-answered", true), ("slack:T1/U-seen", false)))
    }
  }
}

object VoucherContract {

  private def realm(within: String): Realm =
    Realm.of("slack", within).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The realms the contract's voucher records. */
  val T1: Realm = realm("T1")
  val T2: Realm = realm("T2")

  /** The domain the contract's voucher claims. */
  val Claimed: Set[Domain] =
    Set(Domain.of("example.com").fold(e => throw new java.lang.AssertionError(e), identity))

  val Internal: Label = Label.at(Level.Internal)

  val Restricted: Label = Label.at(Level.Restricted)

  /** The one account a group names, `named`. */
  val Named: Account = TestAccounts.account("slack:T1/U-one-a")

  /** Every full member of [[T1]] or [[T2]] is cleared Internal, and [[Named]]'s person
    * Restricted; every room is public.
    */
  val Cleared: Visibility =
    (for {
      compartments <- Compartments.of(Vector.empty).left.map(_.toString)
      v <- Visibility
        .of(
          compartments,
          RoomLabels.Public,
          Vector(
            Group(TestLabels.group("members"), Set.empty, Set(T1, T2)),
            Group(TestLabels.group("named"), Set(Named))
          ),
          Vector(
            Grant(TestLabels.group("members"), Internal),
            Grant(TestLabels.group("named"), Restricted)
          )
        )
        .left
        .map(_.toString)
    } yield v).fold(e => throw new java.lang.AssertionError(e), identity)
}
