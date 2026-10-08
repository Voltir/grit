package grit.dbos.engine

import java.sql.DriverManager
import java.util.concurrent.{CountDownLatch, Executors}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Using

import grit.core.id.{PrincipalId, PrincipalIds, TurnRef, TurnSeq}
import grit.core.identity.{Account, Domain, Realm, Standing, TestAccounts, Vouched}
import grit.core.store.{LastWord, Linking, Origin, StoreError, Voucher}
import grit.core.visibility.{Clearance, Label, Subject}
import grit.dbos.sql.{DbConfig, LiveDb, Opener, TestPostgres}

import utest.*

/** What a trusted realm says of its accounts, recorded by the engine's voucher against a real
  * Postgres: who each account is after, what each vouching reports, and what it writes.
  */
object VouchingLiveTests extends TestSuite {
  import Vouchings.*

  /** A fresh database an engine has started on under [[Seen]], and the voucher it builds for
    * the suite's realms and `domains`.
    */
  private def fresh(suite: String, domains: Set[Domain]): (DbConfig, Voucher) = {
    val c = TestPostgres.freshDatabase(suite)
    val engine = LiveEngine.open(c, "test", visibility = Seen)
    try (c, engine.voucher(Set(T1, T2, T3, Wild, R), domains))
    finally engine.close()
  }

  private lazy val (config, voucher) = fresh("vouching", Claimed)

  /** What `voucher` reports of `account` vouched `standing`, in a transaction of its own; the
    * test fails when it fails, or when the vouching lowered the count of people.
    */
  private def vouch(
      account: Account,
      standing: Standing,
      by: Voucher = voucher,
      in: DbConfig = config
  ): Vector[Linking] = {
    val before = people(in)
    val said = LiveDb
      .under(in, Seen)(by.vouch(Vouched(account, standing)))
      .fold(e => throw new java.lang.AssertionError(s"vouching: $e"), identity)
    assert(people(in) >= before)
    said
  }

  private given DbConfig = config

  /** The person of `address`; fails when none was minted. */
  private def personOf(address: String)(using DbConfig): PrincipalId =
    rows("SELECT principal_id FROM grit.emails WHERE email = ?", address).flatten.headOption
      .fold(throw new java.lang.AssertionError(s"no person for $address"))(PrincipalIds.stored)

  /** `account`'s home. */
  private def homeOf(account: Account)(using DbConfig): String =
    rows(
      "SELECT home FROM grit.identities WHERE account = ?",
      Account.written(account)
    ).flatten.headOption
      .getOrElse(throw new java.lang.AssertionError(s"${Account.written(account)} never seen"))

  /** Who `account` is now. */
  private def whom(account: Account)(using in: DbConfig): String =
    PrincipalId.value(LiveDb.principal(in, account))

  /** The accounts `person` holds now, each as `account:vouched`, in their spelling's order. */
  private def held(person: String)(using DbConfig): Vector[String] =
    rows(
      "SELECT account || ':' || vouched FROM grit.links WHERE principal_id = ? ORDER BY account",
      person
    ).flatten

  /** A turn asked through `by` in a conversation labelled Confidential, above every clearance
    * [[Seen]] grants, so the turn's floor is its asker's clearance.
    */
  private def asked(by: Account, thread: String)(using in: DbConfig): TurnRef = {
    val turn = TurnRef(
      LiveDb.conversation(in, Origin.Slack("T1", "C1", thread), Confidential).id,
      TurnSeq.First
    )
    LiveDb.asking(in, turn, by, None)
    turn
  }

  /** The clearance `turn` opens at, its asker cleared `asker`. */
  private def opensAt(turn: TurnRef, thread: String, asker: Label): Boolean =
    LiveDb.connected(config)(new Opener(Seen).clearance(Subject.Turn(turn), _)) ==
      Right(Clearance.inRoom(Origin.Slack("T1", "C1", thread).room, Confidential, asker))

  private def lastWord(account: Account): LastWord =
    LiveDb
      .transaction(config)(voucher.lastWord(account))
      .fold(e => throw new java.lang.AssertionError(s"$e"), identity)

  val tests = Tests {
    test(
      "a first attestation of a claimed email mints its person, whom the account then is, its home kept"
    ) {
      val a = TestAccounts.account("slack:T1/U-first")
      enrolled(a)
      val home = homeOf(a)
      val before = people(config)
      val said = vouch(a, full("first@example.com"))
      val pE = personOf("first@example.com")
      (said, whom(a), homeOf(a), people(config) - before) ==> (
        Vector(
          Linking.Linked(a, pE, Label.Public, Internal),
          Linking.Standing(a, member = true, Label.Public, Internal)
        ),
        PrincipalId.value(pE),
        home,
        1L
      )
    }

    test("an account never seen is enrolled by its vouching, a home of its own, and attested") {
      val a = TestAccounts.account("slack:T1/U-unseen")
      val said = vouch(a, Standing.Full(None))
      (said, held(homeOf(a)), attestation(a)) ==> (
        Vector(Linking.Standing(a, member = true, Label.Public, Internal)),
        Vector(s"${Account.written(a)}:false"),
        Vector(Vector("", "true"))
      )
    }

    test(
      "accounts of two realms attested one email are one person, cleared by a group naming either"
    ) {
      val v = TestAccounts.account("test:R/V-across")
      vouch(across, full("across@example.com"))
      vouch(v, full("across@example.com"))
      val person = PrincipalId.value(personOf("across@example.com"))
      val turn = asked(v, "3.0")
      (held(person), opensAt(turn, "3.0", Confidential)) ==> (
        Vector(s"${Account.written(across)}:true", s"${Account.written(v)}:true"),
        true
      )
    }

    test(
      "first attestations of one email from two realms at once make one person, holding both, with no deadlock"
    ) {
      val pool = Executors.newFixedThreadPool(20)
      given ExecutionContext = ExecutionContext.fromExecutorService(pool)
      try {
        val pairs = (1 to 10).map { n =>
          (
            TestAccounts.account(s"slack:T2/U-race$n"),
            TestAccounts.account(s"test:R/V-race$n"),
            s"race$n@example.com"
          )
        }.toVector
        val go = new CountDownLatch(1)
        val vouching = Future.sequence(pairs.flatMap { (s, t, address) =>
          Vector(s, t).map { account =>
            Future {
              go.await()
              LiveDb.transaction(config)(voucher.vouch(Vouched(account, full(address))))
            }
          }
        })
        go.countDown()
        val failed = Await.result(vouching, 60.seconds).collect { case Left(e) => e }
        val persons = pairs.map { (s, t, address) =>
          (
            rows("SELECT count(*) FROM grit.emails WHERE email = ?", address).flatten,
            whom(s) == whom(t) && whom(s) == PrincipalId.value(personOf(address))
          )
        }
        (failed, persons) ==> (Vector(), pairs.map(_ => (Vector("1"), true)))
      } finally pool.shutdownNow()
    }

    test(
      "an email outside the claimed domains is not kept, anywhere, and is said unclaimed; the membership is recorded"
    ) {
      val a = TestAccounts.account("slack:T1/U-unclaimed")
      val said = vouch(a, full("x@other.org"))
      (
        said,
        attestation(a),
        whom(a) == homeOf(a),
        rows("SELECT count(*) FROM grit.emails WHERE email LIKE '%other.org'").flatten
      ) ==> (
        Vector(
          Linking.Standing(a, member = true, Label.Public, Internal),
          Linking.Unclaimed(a)
        ),
        Vector(Vector("", "true")),
        true,
        Vector("0")
      )
    }

    test("with no domain claimed nothing links, and a realm's group still clears its member") {
      val (unclaimed, open) = fresh("vouching_unclaimed", Set.empty)
      val a = TestAccounts.account("slack:T1/U-nodomain")
      val b = TestAccounts.account("slack:T1/U-nodomain-b")
      val said = vouch(a, full("same@example.com"), open, unclaimed)
      vouch(b, full("same@example.com"), open, unclaimed)
      val turn = asked(a, "6.0")(using unclaimed)
      val opened = LiveDb.connected(unclaimed)(new Opener(Seen).clearance(Subject.Turn(turn), _))
      (
        said,
        whom(a)(using unclaimed) == homeOf(a)(using unclaimed),
        whom(b)(using unclaimed) == homeOf(b)(using unclaimed),
        rows("SELECT count(*) FROM grit.emails")(using unclaimed).flatten,
        opened
      ) ==> (
        Vector(Linking.Standing(a, member = true, Label.Public, Internal), Linking.Unclaimed(a)),
        true,
        true,
        Vector("0"),
        Right(Clearance.inRoom(Origin.Slack("T1", "C1", "6.0").room, Confidential, Internal))
      )
    }

    test(
      "a changed email moves the account from the first email's person to the second's in one vouching, and no other account"
    ) {
      val a = TestAccounts.account("slack:T2/U-m1-a")
      vouch(named, full("m1-first@example.com"))
      val linked = vouch(a, full("m1-first@example.com"))
      val pE1 = personOf("m1-first@example.com")
      val moved = vouch(a, full("m1-second@example.com"))
      val pE2 = personOf("m1-second@example.com")
      (linked, moved, whom(named), whom(a)) ==> (
        Vector(
          Linking.Linked(a, pE1, Label.Public, Confidential),
          Linking.Standing(a, member = true, Label.Public, Confidential)
        ),
        Vector(
          Linking.Unlinked(a, pE1, Confidential, Label.Public),
          Linking.Linked(a, pE2, Confidential, Label.Public)
        ),
        PrincipalId.value(pE1),
        PrincipalId.value(pE2)
      )
    }

    test(
      "outside its realm's membership an account is back at the same home, no member; a member with no email is back home a member"
    ) {
      val a = TestAccounts.account("slack:T1/U-withdrawn")
      val c = TestAccounts.account("slack:T1/U-withdrawn-c")
      enrolled(a)
      val home = homeOf(a)
      vouch(a, full("withdrawn@example.com"))
      vouch(c, full("withdrawn-c@example.com"))
      val pE = personOf("withdrawn@example.com")
      val pC = personOf("withdrawn-c@example.com")
      val outside = vouch(a, Standing.Outside)
      val noEmail = vouch(c, Standing.Full(None))
      (outside, whom(a), attestation(a), noEmail, attestation(c)) ==> (
        Vector(
          Linking.Unlinked(a, pE, Internal, Label.Public),
          Linking.Standing(a, member = false, Internal, Label.Public)
        ),
        home,
        Vector(Vector("", "false")),
        Vector(Linking.Unlinked(c, pC, Internal, Internal)),
        Vector(Vector("", "true"))
      )
    }

    test("an account no realm of the voucher holds is said outside, and nothing is recorded") {
      val a = TestAccounts.account("slack:T9/U-outside")
      (
        vouch(a, full("outside@example.com")),
        attestation(a),
        rows(
          "SELECT count(*) FROM grit.emails WHERE email = 'outside@example.com'"
        ).flatten
      ) ==> (Vector(Linking.Outside(a)), Vector(), Vector("0"))
    }

    test("the same answer again says nothing changed, and is its realm's last word now") {
      val a = TestAccounts.account("slack:T1/U-renewed")
      vouch(a, full("renewed@example.com"))
      aged(a, "1 hour")
      val stale = lastWord(a).ago.exists(_ >= 1.hour)
      val again = vouch(a, full("renewed@example.com"))
      (stale, again, lastWord(a).ago.exists(_ < 1.second)) ==> (true, Vector(), true)
    }

    test("a renewal is a heap-only update: a hundred renew without writing an index") {
      val (hot, open) = fresh("vouching_hot", Claimed)
      val a = TestAccounts.account("slack:T1/U-hot")
      vouch(a, full("hot@example.com"), open, hot)
      Using.resource(DriverManager.getConnection(hot.jdbcUrl, hot.user, hot.password)) { conn =>
        conn.setAutoCommit(false)
        (1 to 100).foreach { _ =>
          open
            .vouch(Vouched(a, full("hot@example.com")))(using
              LiveDb.opened(new Opener(Seen).maintained(conn))
            )
            .fold(e => throw new java.lang.AssertionError(s"renewing: $e"), identity)
          conn.commit()
        }
        Using.resource(conn.prepareStatement("SELECT pg_stat_force_next_flush()"))(ps => {
          val _ = ps.execute()
        })
        conn.commit()
      }
      // Each session flushes its counts when idle or closed: read until every update is counted.
      def counts(): (Long, Long) =
        rows(
          """SELECT n_tup_upd, n_tup_hot_upd FROM pg_stat_user_tables
            | WHERE relid = 'grit.attestations'::regclass""".stripMargin
        )(using hot).headOption.fold((0L, 0L))(r => (r(0).toLong, r(1).toLong))
      val deadline = System.currentTimeMillis() + 10000
      while (counts()._1 < 100 && System.currentTimeMillis() < deadline) Thread.sleep(100)
      counts() ==> (100L, 100L)
    }

    test(
      "vouchings of two accounts of one email, in opposite orders at once, never deadlock"
    ) {
      val a = TestAccounts.account("slack:T1/U-lock-a")
      val b = TestAccounts.account("slack:T1/U-lock-b")
      val pool = Executors.newFixedThreadPool(2)
      given ExecutionContext = ExecutionContext.fromExecutorService(pool)
      try {
        def rounds(first: Account, second: Account): Future[Vector[StoreError]] = Future {
          (1 to 50).toVector.flatMap { _ =>
            LiveDb
              .transaction(config)(for {
                one <- voucher.vouch(Vouched(first, full("lock@example.com")))
                two <- voucher.vouch(Vouched(second, full("lock@example.com")))
              } yield one ++ two)
              .left
              .toOption
          }
        }
        val both = Future.sequence(Vector(rounds(a, b), rounds(b, a)))
        Await.result(both, 120.seconds).flatten ==> Vector()
      } finally pool.shutdownNow()
    }

    test(
      "a realm's last words are every account of it seen, never attested ones without an age, and none of another realm"
    ) {
      val w1 = TestAccounts.account("slack:T3/U-w1")
      val w2 = TestAccounts.account("slack:T3/U-w2")
      val longer = TestAccounts.account("slack:T30/U-w3")
      val wild = TestAccounts.account("slack:T_/U-w4")
      val matched = TestAccounts.account("slack:TX/U-w5")
      vouch(w1, Standing.Full(None))
      enrolled(w2)
      enrolled(longer)
      vouch(wild, Standing.Full(None))
      enrolled(matched)
      def words(of: Realm): Set[(Account, Boolean)] =
        LiveDb
          .transaction(config)(voucher.lastWords(of))
          .fold(e => throw new java.lang.AssertionError(s"$e"), identity)
          .map(w => (w.account, w.ago.isDefined))
          .toSet
      (words(T3), words(Wild)) ==> (Set((w1, true), (w2, false)), Set((wild, true)))
    }

    test(
      "a realm's word that an account is a full member clears its turns through the realm's group, until it says otherwise"
    ) {
      val u = TestAccounts.account("slack:T1/U-member")
      val turn = asked(u, "14.0")
      val first = opensAt(turn, "14.0", Label.Public)
      val member = vouch(u, Standing.Full(None))
      val cleared = opensAt(turn, "14.0", Internal)
      val outside = vouch(u, Standing.Outside)
      (
        first,
        member,
        cleared,
        outside,
        opensAt(turn, "14.0", Label.Public)
      ) ==> (
        true,
        Vector(Linking.Standing(u, member = true, Label.Public, Internal)),
        true,
        Vector(Linking.Standing(u, member = false, Internal, Label.Public)),
        true
      )
    }
  }
}
