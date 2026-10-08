package grit.core.edge

import scala.concurrent.duration.*

import grit.core.identity.{Account, Realm, Standing, Vouched}
import grit.core.store.{Jot, LastWord, Linking, StoreError, Voucher}
import grit.core.visibility.Subject

/** What a trusted realm's source says of one account when asked ([[RealmSource.ask]]). */
enum Asked {

  /** Its word: the account's standing, and the email it verified, as it states them. */
  case Said(standing: Standing)

  /** It could not be asked at all (down, rate-limited, a permission withdrawn), `why` as it
    * said; never an answer about the account.
    */
  case Unreached(why: String)
}

/** A source a deployment trusts to say who a realm's accounts are (ADR 0032): a chat
  * workspace's members, a company directory, a login's identity provider. It reports what the
  * source states and decides nothing: [[Attesting]] decides when it is asked, and which emails
  * count. The edge that attests a realm implements it, as its own source.
  */
trait RealmSource {

  /** What the source says of `account` now: [[Asked.Said]] with [[Standing.Outside]] when it
    * knows no such account, or will not say who it is; [[Asked.Unreached]] only when it could
    * not be asked at all.
    */
  def ask(account: Account): Asked

  /** What the source says now of every account it knows in `realm`, read whole: an account it
    * does not list is one it does not know. `Left` when any part of the listing could not be
    * read, never a partial list.
    */
  def all(realm: Realm): Either[Asked.Unreached, Map[Account, Standing]]
}

/** grit's rules for asking trusted realms' sources who their accounts are, and for recording
  * each answer (ADR 0032), for the realms `voucher` records. What a source said stands until it
  * says otherwise, or until the deployment's start withdraws its trust; its age, by the
  * database's clock, only makes it due to be asked again. Each answer is recorded in a
  * transaction of its own, written with `jot`, so no two accounts' locks are ever held at once,
  * and a source is never asked inside a transaction. Each change an answer makes, and each time
  * a source could not be reached, is told to `report` as it happens, but for a message's
  * account it never answered for, which [[before]] returns, as each method returns a store
  * failure. The kit builds one per edge, over the realms the
  * deployment trusts that edge's attester for: for an edge that attests nothing, it asks
  * nothing.
  */
final class Attesting(voucher: Voucher, jot: Jot, report: Attesting.Report => Unit) {

  /** Asks `source` what it says of `account` now and records the answer, before a message
    * written through `account` is recorded or heard, so the turn it starts opens under it.
    * Asks nothing for an account of no realm this edge attests, or one answered for less than
    * [[Attesting.Fresh]] ago. When `source` cannot be reached, an account it has answered for
    * keeps that answer, reported; one it never has is `Unattested`, and its message must not be
    * recorded, so that its source sends it again.
    */
  def before(source: RealmSource^, account: Account): Either[Attesting.Unchecked, Unit] =
    realmOf(account).fold(Right(())) { realm =>
      jot.write(Subject.Public)(voucher.lastWord(account)) match {
        case Left(e) => Left(Attesting.Unchecked.Store(e))
        case Right(LastWord(_, Some(ago))) if ago < Attesting.Fresh => Right(())
        case Right(LastWord(_, ago)) =>
          source.ask(account) match {
            case Asked.Said(standing) =>
              recorded(Vouched(account, standing)).left.map(Attesting.Unchecked.Store(_))
            case Asked.Unreached(why) if ago.isDefined =>
              Right(report(Attesting.Report.Unreached(realm, 1, why)))
            case Asked.Unreached(why) => Left(Attesting.Unchecked.Unattested(account, why))
          }
      }
    }

  /** As [[before]], for an account `source` reported changed unasked, however recently it was
    * answered for. When `source` cannot be reached, what it said before stands, reported, and a
    * look asks again once it is due.
    */
  def changed(source: RealmSource^, account: Account): Either[StoreError, Unit] =
    realmOf(account).fold(Right(())) { realm =>
      source.ask(account) match {
        case Asked.Said(standing) => recorded(Vouched(account, standing))
        case Asked.Unreached(why) => Right(report(Attesting.Report.Unreached(realm, 1, why)))
      }
    }

  /** One look: for each realm this edge attests where an account seen was never answered for,
    * or was last answered [[Attesting.Due]] ago or more, lists the realm once
    * ([[RealmSource.all]]) and records every account seen there as listed, one not listed as
    * [[Standing.Outside]], in the order of their spelling; an account listed but never seen is
    * not recorded. When `source` cannot list a realm, nothing of it is recorded, and its due
    * accounts are reported, with an alarm when any has gone unanswered `2 × Due` or more, or
    * never; the next look asks again. How many accounts it recorded; a store failure stops the
    * look, keeping what it recorded before.
    */
  def round(source: RealmSource^): Either[StoreError, Int] =
    voucher.realms.toVector
      .sortBy(r => (r.namespace, r.within))
      .foldLeft[Either[StoreError, Int]](Right(0)) { (so, realm) =>
        so.flatMap(n => look(source, realm).map(n + _))
      }

  /** [[round]] for `realm` alone. */
  private def look(source: RealmSource^, realm: Realm): Either[StoreError, Int] =
    jot.write(Subject.Public)(voucher.lastWords(realm)).flatMap { words =>
      val due = words.filter(_.ago.forall(_ >= Attesting.Due))
      if (due.isEmpty) Right(0)
      else
        source.all(realm) match {
          case Left(Asked.Unreached(why)) =>
            report(Attesting.Report.Unreached(realm, due.size, why))
            val overdue = due.count(_.ago.forall(_ >= Attesting.Due * 2))
            if (overdue > 0) report(Attesting.Report.Overdue(realm, overdue, why))
            Right(0)
          case Right(listed) =>
            words
              .map(_.account)
              .sortBy(Account.written)
              .foldLeft[Either[StoreError, Int]](Right(0)) { (so, account) =>
                so.flatMap { n =>
                  recorded(Vouched(account, listed.getOrElse(account, Standing.Outside)))
                    .map(_ => n + 1)
                }
              }
        }
    }

  /** The realm this edge attests that holds `account`, if any. */
  private def realmOf(account: Account): Option[Realm] = voucher.realms.find(_.holds(account))

  /** `vouched` recorded in a transaction of its own, each change it made reported. */
  private def recorded(vouched: Vouched): Either[StoreError, Unit] =
    jot
      .write(Subject.Public)(voucher.vouch(vouched))
      .map(_.foreach(l => report(Attesting.Report.Changed(l))))
}

object Attesting {

  /** 24 hours: how long a source's answer stands before a look asks again. It never ends one. */
  val Due: FiniteDuration = 24.hours

  /** One minute: how recent an answer spares a message's account from being asked again. */
  val Fresh: FiniteDuration = 1.minute

  /** Ten minutes: how often the kit asks each attesting edge for a look
    * ([[ServedEdge.Open.attest]]).
    */
  val Every: FiniteDuration = 10.minutes

  /** What a check or a look tells the log. */
  enum Report {

    /** An answer's recording said `linking`: a change to its account's person or membership,
      * or why its email was not kept. Information.
      */
    case Changed(linking: Linking)

    /** The source of `realm` could not be asked about `accounts` of its accounts, `why`; what
      * it said before stands. A warning.
      */
    case Unreached(realm: Realm, accounts: Int, why: String)

    /** `overdue` accounts of `realm` have had no answer for `2 × Due` or more, or ever, `why`.
      * An error, at every look until the source answers.
      */
    case Overdue(realm: Realm, overdue: Int, why: String)

    /** A line a person reads, naming accounts and realms, never an address. */
    def message: String = this match {
      case Changed(linking) => linking.message
      case Unreached(realm, accounts, why) =>
        s"${Report.spelled(realm)} could not be asked about $accounts of its accounts ($why); what it said before stands"
      case Overdue(realm, overdue, why) =>
        s"${Report.spelled(realm)}: $overdue of its accounts have had no answer for ${Due * 2} or more, or ever ($why)"
    }
  }

  object Report {

    /** `realm` as its accounts' spelling begins. */
    private def spelled(realm: Realm): String = s"${realm.namespace}:${realm.within}/"
  }

  /** Why a message's account was not checked ([[Attesting.before]]). */
  enum Unchecked {

    /** Its source could not be asked, `why`, and has never answered for it. */
    case Unattested(account: Account, why: String)

    /** The store could not be read or written. */
    case Store(error: StoreError)
  }
}
