package grit.dbos.engine

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.edge.{Asked, Attesting, RealmSource}
import grit.core.identity.{Account, Realm, Standing, TestAccounts, Vouched}
import grit.core.store.{LastWord, Linking, StoreError, Tx, Voucher}
import grit.dbos.sql.{DbConfig, LiveDb, Opener, SqlJot, SqlVoucher, TestPostgres}

import org.postgresql.ds.PGSimpleDataSource
import utest.*

/** Core's rules for asking a trusted realm's source who its accounts are, over the engine's
  * voucher against a real Postgres: when the source is asked, what each answer and each failure
  * records, and what is reported. Each test attests a realm of its own.
  */
object AttestingLiveTests extends TestSuite {
  import Vouchings.*

  private lazy val config: DbConfig = {
    val c = TestPostgres.freshDatabase("attesting")
    LiveEngine.open(c, "test", visibility = Seen).close()
    c
  }

  private given DbConfig = config

  /** A source the test scripts: what it says of each account, or why it cannot be reached; it
    * counts what it was asked, and the calls made while another session held a lock on the
    * identity tables, as a transaction left open around the call would.
    */
  private final class Scripted extends RealmSource {
    // Each var holds an immutable value, read and written only on the test's own thread.
    @caps.unsafe.untrackedCaptures
    var says: Map[Account, Standing] = Map.empty
    @caps.unsafe.untrackedCaptures
    var down: Option[String] = None
    @caps.unsafe.untrackedCaptures
    var asked: Vector[Account] = Vector.empty
    @caps.unsafe.untrackedCaptures
    var listed: Int = 0
    @caps.unsafe.untrackedCaptures
    var inside: Int = 0

    def ask(account: Account): Asked = {
      held()
      asked = asked :+ account
      down.fold(Asked.Said(says.getOrElse(account, Standing.Outside)))(Asked.Unreached(_))
    }

    def all(realm: Realm): Either[Asked.Unreached, Map[Account, Standing]] = {
      held()
      listed += 1
      down.fold(Right(says.filter((a, _) => realm.holds(a))))(why => Left(Asked.Unreached(why)))
    }

    private def held(): Unit =
      if (
        rows(
          """SELECT count(*) FROM pg_locks l
            | WHERE l.database = (SELECT oid FROM pg_database WHERE datname = current_database())
            |   AND l.relation IN ('grit.attestations'::regclass, 'grit.identities'::regclass)
            |   AND l.pid <> pg_backend_pid()""".stripMargin
        ).flatten != Vector("0")
      ) inside += 1
  }

  /** `inner`, but its third vouching fails, as a database lost part-way through a look would. */
  private final class FailingThird(inner: Voucher) extends Voucher {
    private val vouchings = new AtomicInteger(0)
    def realms: Set[Realm] = inner.realms
    def vouch(vouched: Vouched)(using Tx^): Either[StoreError, Vector[Linking]] =
      if (vouchings.incrementAndGet() == 3) Left(StoreError.DatabaseError("the third fails"))
      else inner.vouch(vouched)
    def lastWord(account: Account)(using Tx^): Either[StoreError, LastWord] =
      inner.lastWord(account)
    def lastWords(realm: Realm)(using Tx^): Either[StoreError, Vector[LastWord]] =
      inner.lastWords(realm)
  }

  /** Core's rules over the engine's voucher for `realm` alone, with the claimed domain,
    * reporting to `reports`, the voucher wrapped by `wrap`.
    */
  private def attesting(
      realm: Realm,
      reports: ConcurrentLinkedQueue[Attesting.Report],
      wrap: Voucher -> Voucher = v => v
  ): Attesting^ = {
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    new Attesting(
      wrap(new SqlVoucher(Set(realm), Claimed)),
      new SqlJot(ds, new Opener(Seen)),
      r => { val _ = reports.add(r) }
    )
  }

  private def said(reports: ConcurrentLinkedQueue[Attesting.Report]): Vector[Attesting.Report] =
    reports.asScala.toVector

  /** How long ago `account` was last answered for, by the database's clock. */
  private def ago(account: Account): Option[FiniteDuration] =
    LiveDb
      .transaction(config)(new SqlVoucher(Set.empty, Set.empty).lastWord(account))
      .fold(e => throw new java.lang.AssertionError(s"$e"), _.ago)

  private def recent(account: Account): Boolean = ago(account).exists(_ < 5.seconds)

  private def identities(): String =
    rows("SELECT count(*) FROM grit.identities").flatten.mkString

  val tests = Tests {
    test(
      "before a message, its account is asked once and recorded; within Fresh it is not asked again, and an account of no realm attested is never asked"
    ) {
      val realm = Vouchings.realm("slack", "TB1")
      val a = TestAccounts.account("slack:TB1/U1")
      val other = TestAccounts.account("slack:TX9/U1")
      val source = new Scripted
      source.says = Map(a -> full("before@example.com"))
      val reports = new ConcurrentLinkedQueue[Attesting.Report]
      val checks = attesting(realm, reports)
      val first = checks.before(source, a)
      val again = checks.before(source, a)
      val elsewhere = checks.before(source, other)
      (first, again, elsewhere, source.asked, attestation(a)) ==> (
        Right(()),
        Right(()),
        Right(()),
        Vector(a),
        Vector(Vector("before@example.com", "true"))
      )
    }

    test("an answer just under Fresh old spares the account; one just over it is asked") {
      val realm = Vouchings.realm("slack", "TB2")
      val young = TestAccounts.account("slack:TB2/U-young")
      val old = TestAccounts.account("slack:TB2/U-old")
      val source = new Scripted
      source.says = Map(young -> Standing.Full(None), old -> Standing.Full(None))
      val checks = attesting(realm, new ConcurrentLinkedQueue)
      checks.before(source, young)
      checks.before(source, old)
      aged(young, s"${(Attesting.Fresh - 5.seconds).toSeconds} seconds")
      aged(old, s"${(Attesting.Fresh + 5.seconds).toSeconds} seconds")
      source.asked = Vector.empty
      (checks.before(source, young), checks.before(source, old), source.asked) ==> (
        Right(()),
        Right(()),
        Vector(old)
      )
    }

    test(
      "a source unreached before a message keeps an answered account's last word, reported once"
    ) {
      val realm = Vouchings.realm("slack", "TB3")
      val a = TestAccounts.account("slack:TB3/U1")
      val source = new Scripted
      source.says = Map(a -> full("kept@example.com"))
      val reports = new ConcurrentLinkedQueue[Attesting.Report]
      val checks = attesting(realm, reports)
      checks.before(source, a)
      aged(a, "2 minutes")
      reports.clear()
      source.down = Some("ratelimited")
      (checks.before(source, a), attestation(a), said(reports)) ==> (
        Right(()),
        Vector(Vector("kept@example.com", "true")),
        Vector(Attesting.Report.Unreached(realm, 1, "ratelimited"))
      )
    }

    test(
      "a source unreached for an account it never answered for refuses its message, writing nothing"
    ) {
      val realm = Vouchings.realm("slack", "TB4")
      val a = TestAccounts.account("slack:TB4/U1")
      val source = new Scripted
      source.down = Some("invalid_auth")
      val checks = attesting(realm, new ConcurrentLinkedQueue)
      (checks.before(source, a), attestation(a)) ==> (
        Left(Attesting.Unchecked.Unattested(a, "invalid_auth")),
        Vector()
      )
    }

    test(
      "a change the source reports is asked within Fresh; unreached, it stands and is reported"
    ) {
      val realm = Vouchings.realm("slack", "TB5")
      val a = TestAccounts.account("slack:TB5/U1")
      val source = new Scripted
      source.says = Map(a -> full("change@example.com"))
      val reports = new ConcurrentLinkedQueue[Attesting.Report]
      val checks = attesting(realm, reports)
      checks.before(source, a)
      source.says = Map(a -> Standing.Outside)
      val changed = checks.changed(source, a)
      val after = attestation(a)
      reports.clear()
      source.down = Some("network")
      val unreached = checks.changed(source, a)
      (changed, after, source.asked, unreached, attestation(a), said(reports)) ==> (
        Right(()),
        Vector(Vector("", "false")),
        Vector(a, a, a),
        Right(()),
        Vector(Vector("", "false")),
        Vector(Attesting.Report.Unreached(realm, 1, "network"))
      )
    }

    test(
      "a look with nothing due lists nothing; one with an account due lists its realm once, records every seen account as listed and one not listed as outside, and the next lists nothing"
    ) {
      val realm = Vouchings.realm("slack", "TL1")
      val a = TestAccounts.account("slack:TL1/U-a")
      val b = TestAccounts.account("slack:TL1/U-b")
      val gone = TestAccounts.account("slack:TL1/U-gone")
      val source = new Scripted
      source.says =
        Map(a -> Standing.Full(None), b -> Standing.Full(None), gone -> Standing.Full(None))
      val reports = new ConcurrentLinkedQueue[Attesting.Report]
      val checks = attesting(realm, reports)
      Vector(a, b, gone).foreach(checks.before(source, _))
      val idle = (checks.round(source), source.listed)
      aged(a, "25 hours")
      // Not due, so only a look that rewrites every seen account makes it recent again.
      aged(b, "1 hour")
      source.says = Map(a -> Standing.Full(None), b -> Standing.Full(None))
      reports.clear()
      val look = checks.round(source)
      val after = (source.listed, Vector(a, b, gone).map(recent), attestation(gone))
      val reported = said(reports).collect {
        case Attesting.Report.Changed(Linking.Standing(account, member, _, _)) => (account, member)
      }
      val next = (checks.round(source), source.listed)
      (idle, look, after, reported, next) ==> (
        (Right(0), 0),
        Right(3),
        (1, Vector(true, true, true), Vector(Vector("", "false"))),
        Vector((gone, false)),
        (Right(0), 1)
      )
    }

    test("an answer just under Due old is not due; one just over it is") {
      val realm = Vouchings.realm("slack", "TL2")
      val a = TestAccounts.account("slack:TL2/U1")
      val source = new Scripted
      source.says = Map(a -> Standing.Full(None))
      val checks = attesting(realm, new ConcurrentLinkedQueue)
      checks.before(source, a)
      aged(a, s"${(Attesting.Due - 5.seconds).toSeconds} seconds")
      val under = (checks.round(source), source.listed)
      aged(a, s"${(Attesting.Due + 5.seconds).toSeconds} seconds")
      val over = (checks.round(source), source.listed)
      (under, over) ==> ((Right(0), 0), (Right(1), 1))
    }

    test("a look never enrols an account its listing names that grit has not seen") {
      val realm = Vouchings.realm("slack", "TL3")
      val a = TestAccounts.account("slack:TL3/U-seen")
      val stranger = TestAccounts.account("slack:TL3/U-stranger")
      val source = new Scripted
      source.says = Map(a -> Standing.Full(None), stranger -> Standing.Full(None))
      val checks = attesting(realm, new ConcurrentLinkedQueue)
      checks.before(source, a)
      aged(a, "25 hours")
      val before = identities()
      (checks.round(source), identities(), attestation(stranger)) ==> (Right(1), before, Vector())
    }

    test(
      "a look whose store fails on its third account keeps the two before it, and the next look records the rest"
    ) {
      val realm = Vouchings.realm("slack", "TL4")
      val accounts = (1 to 5).toVector.map(n => TestAccounts.account(s"slack:TL4/U$n"))
      accounts.foreach(enrolled)
      val source = new Scripted
      source.says = accounts.map(_ -> Standing.Full(None)).toMap
      val checks = attesting(realm, new ConcurrentLinkedQueue, new FailingThird(_))
      val failed = checks.round(source)
      val kept = accounts.map(recent)
      val next = checks.round(source)
      (failed, kept, next, accounts.map(recent)) ==> (
        Left(StoreError.DatabaseError("the third fails")),
        Vector(true, true, false, false, false),
        Right(5),
        Vector.fill(5)(true)
      )
    }

    test(
      "a look whose source is unreached writes nothing and reports the due accounts, with an alarm at every look while one is twice Due old"
    ) {
      val realm = Vouchings.realm("slack", "TL5")
      val due = TestAccounts.account("slack:TL5/U-due")
      val overdue = TestAccounts.account("slack:TL5/U-overdue")
      val source = new Scripted
      source.says = Map(due -> Standing.Full(None), overdue -> Standing.Full(None))
      val reports = new ConcurrentLinkedQueue[Attesting.Report]
      val checks = attesting(realm, reports)
      Vector(due, overdue).foreach(checks.before(source, _))
      aged(due, "25 hours")
      aged(overdue, "49 hours")
      reports.clear()
      source.down = Some("ratelimited")
      val first = checks.round(source)
      val firstSaid = said(reports)
      reports.clear()
      val second = checks.round(source)
      (first, firstSaid, second, said(reports), Vector(due, overdue).map(recent)) ==> (
        Right(0),
        Vector(
          Attesting.Report.Unreached(realm, 2, "ratelimited"),
          Attesting.Report.Overdue(realm, 1, "ratelimited")
        ),
        Right(0),
        Vector(
          Attesting.Report.Unreached(realm, 2, "ratelimited"),
          Attesting.Report.Overdue(realm, 1, "ratelimited")
        ),
        Vector(false, false)
      )
    }

    test(
      "an account attested three weeks ago, its source unreached throughout, is still its email's person; reached, a look renews it"
    ) {
      val realm = Vouchings.realm("slack", "TL6")
      val a = TestAccounts.account("slack:TL6/U-away")
      val source = new Scripted
      source.says = Map(a -> full("away@example.com"))
      val checks = attesting(realm, new ConcurrentLinkedQueue)
      LiveDb
        .transaction(config)(
          new SqlVoucher(Set(realm), Claimed).vouch(Vouched(a, full("away@example.com")))
        )
        .fold(e => throw new java.lang.AssertionError(s"vouching: $e"), identity)
      val person = LiveDb.principal(config, a)
      aged(a, "21 days")
      source.down = Some("network")
      val away = (checks.round(source), checks.before(source, a), LiveDb.principal(config, a))
      source.down = None
      val back = (checks.round(source), recent(a), LiveDb.principal(config, a))
      (away, back) ==> ((Right(0), Right(()), person), (Right(1), true, person))
    }

    test("no source is asked while a transaction is open on the identity tables") {
      val realm = Vouchings.realm("slack", "TL7")
      val a = TestAccounts.account("slack:TL7/U1")
      val b = TestAccounts.account("slack:TL7/U2")
      val source = new Scripted
      source.says = Map(a -> full("inside@example.com"), b -> Standing.Full(None))
      val checks = attesting(realm, new ConcurrentLinkedQueue)
      checks.before(source, a)
      checks.changed(source, a)
      enrolled(b)
      checks.round(source)
      (source.asked, source.listed, source.inside) ==> (Vector(a, a), 1, 0)
    }
  }
}
