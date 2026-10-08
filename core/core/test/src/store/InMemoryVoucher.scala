package grit.core.store

import scala.concurrent.duration.{Duration, FiniteDuration}

import grit.core.id.{PrincipalId, TestPrincipalIds}
import grit.core.identity.{
  Account,
  Domain,
  Email,
  Evidence,
  Held,
  Principal,
  Realm,
  Standing,
  Vouched
}
import grit.core.visibility.Visibility

/** An in-memory [[Voucher]] for tests, keeping [[VoucherContract]], for `realms`, keeping an
  * email only in one of `domains`, and saying each change's clearances as `visibility` clears
  * people. It ignores the `Tx`, and holds the accounts it has seen itself ([[saw]]).
  */
final class InMemoryVoucher(val realms: Set[Realm], domains: Set[Domain], visibility: Visibility)
    extends Voucher {
  import InMemoryVoucher.Said

  // Each var holds an immutable value, written and read only on the test's own thread, through
  // the calls it makes and waits on.
  @caps.unsafe.untrackedCaptures
  private var homes = Map.empty[Account, PrincipalId]

  @caps.unsafe.untrackedCaptures
  private var attested = Map.empty[Account, Said]

  @caps.unsafe.untrackedCaptures
  private var persons = Map.empty[Email, PrincipalId]

  @caps.unsafe.untrackedCaptures
  private var minted = 0

  /** Every answer given for an account of [[realms]], in order. */
  @caps.unsafe.untrackedCaptures
  var vouched = Vector.empty[Vouched]

  /** Has seen `account`, as a first message through it would: a new person's one account. */
  def saw(account: Account): Unit =
    if (!homes.contains(account)) homes = homes.updated(account, mint())

  /** Arranges `account`'s last answer as given `ago` before now; nothing when it has none. */
  def aged(account: Account, ago: FiniteDuration): Unit =
    attested.get(account).foreach(s => attested = attested.updated(account, s.copy(ago = ago)))

  def vouch(v: Vouched)(using Tx^): Either[StoreError, Vector[Linking]] = {
    val account = v.account
    if (!realms.exists(_.holds(account))) Right(Vector(Linking.Outside(account)))
    else {
      vouched = vouched :+ v
      val (kept, member, unclaimed) = v.standing match {
        case Standing.Full(Some(e)) if domains.contains(Email.domain(e)) => (Some(e), true, false)
        case Standing.Full(Some(_)) => (None, true, true)
        case Standing.Full(None) => (None, true, false)
        case Standing.Outside => (None, false, false)
      }
      kept.foreach(e => if (!persons.contains(e)) persons = persons.updated(e, mint()))
      saw(account)
      val was = attested.get(account)
      val now = Said(kept, member, Duration.Zero)
      attested = attested.updated(account, now)
      if (was.exists(w => w.email == kept && w.member == member)) Right(Vector.empty)
      else
        changes(account, was.getOrElse(Said(None, member = false, Duration.Zero)), now)
          .map(c => if (unclaimed) c :+ Linking.Unclaimed(account) else c)
    }
  }

  def lastWord(account: Account)(using Tx^): Either[StoreError, LastWord] =
    Right(LastWord(account, attested.get(account).map(_.ago)))

  def lastWords(realm: Realm)(using Tx^): Either[StoreError, Vector[LastWord]] =
    Right(
      homes.keySet.filter(realm.holds).toVector.map(a => LastWord(a, attested.get(a).map(_.ago)))
    )

  private def mint(): PrincipalId = {
    minted += 1
    TestPrincipalIds.stored(s"person-$minted")
  }

  /** What moving `account` from `was` to `now` did to its person and membership. */
  private def changes(account: Account, was: Said, now: Said): Either[StoreError, Vector[Linking]] =
    for {
      before <- before(account, was)
      after <- whom(account).map(id => (id, held(id)))
    } yield {
      val (from, to) = (before._1, after._1)
      val wasCleared = visibility.cleared(Principal.Person(from, before._2))
      val nowCleared = visibility.cleared(Principal.Person(to, after._2))
      Vector(
        Option.when(was.email.isDefined && from != to)(
          Linking.Unlinked(account, from, wasCleared, nowCleared)
        ),
        Option.when(now.email.isDefined && from != to)(
          Linking.Linked(account, to, wasCleared, nowCleared)
        ),
        Option.when(was.member != now.member)(
          Linking.Standing(account, now.member, wasCleared, nowCleared)
        )
      ).flatten
    }

  /** Whom `account` was while its attestation said `was`, holding the accounts they hold now
    * but `account`, and `account` as it was.
    */
  private def before(account: Account, was: Said): Either[StoreError, (PrincipalId, Set[Held])] = {
    val as = Held(account, if (was.email.isDefined) Evidence.Vouched else Evidence.Home, was.member)
    was.email match {
      case Some(e) =>
        persons
          .get(e)
          .toRight(StoreError.Invalid(s"no person for ${Email.value(e)}"))
          .map(id => (id, held(id).filterNot(_.account == account) + as))
      case None =>
        homes
          .get(account)
          .toRight(StoreError.Invalid(s"${Account.written(account)} never seen"))
          .map(id => (id, Set(as)))
    }
  }

  /** Whom `account` is now: its attested email's person, else its home. */
  private def whom(account: Account): Either[StoreError, PrincipalId] =
    attested
      .get(account)
      .flatMap(_.email)
      .flatMap(persons.get)
      .orElse(homes.get(account))
      .toRight(StoreError.Invalid(s"${Account.written(account)} is no one"))

  /** Every account `person` holds now. */
  private def held(person: PrincipalId): Set[Held] =
    homes.keySet
      .filter(a => whom(a) == Right(person))
      .map { a =>
        val said = attested.get(a)
        Held(
          a,
          if (said.exists(_.email.isDefined)) Evidence.Vouched else Evidence.Home,
          said.exists(_.member)
        )
      }
}

private object InMemoryVoucher {

  /** What an account's realm last said of it: its kept email, its membership, and how long ago. */
  final case class Said(email: Option[Email], member: Boolean, ago: FiniteDuration)
}
