package grit.dbos.engine

import java.time.Instant
import java.util.UUID
import java.util.concurrent.{CountDownLatch, Executors}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Try, Using}

import grit.core.id.{ConversationId, PrincipalId, SourceId, TurnRef, TurnSeq}
import grit.core.identity.{Account, TestAccounts}
import grit.core.inbox.{InboundId, InboxError}
import grit.core.message.Message
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.visibility.Subject
import grit.dbos.sql.{
  LiveDb,
  SqlConversationStore,
  SqlIdentities,
  SqlPrincipals,
  SqlReviews,
  SqlSpeechStore,
  TestPostgres
}

import utest.*

/** Each account an action comes through, as `grit.identities` keeps it, against a real
  * Postgres.
  */
object IdentitiesLiveTests extends TestSuite {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("identities")
    LiveEngine.open(c, "test").close()
    c
  }

  private val ana = TestAccounts.account("slack:T1/UA")
  private val ben = TestAccounts.account("slack:T1/UB")

  private def ingest(engine: Engine^, thread: String, by: Account): Either[InboxError, TurnRef] =
    engine.inbox.ingest(
      Origin.Slack("T1", "C1", thread),
      SourceId(thread),
      Message.User("hello"),
      by
    )

  /** Who wrote the message `turn` began with, as a room's reads name them. */
  private def author(engine: Engine^, turn: TurnRef, thread: String): Option[PrincipalId] =
    LiveDb
      .transaction(config)(
        engine.rooms.author(InboundId.of(turn.conversationId, SourceId(thread)))
      )
      .fold(e => throw new java.lang.AssertionError(s"reading an author: $e"), identity)

  /** The version of the UUID `id` spells; `None` when it spells none. */
  private def uuidVersion(id: PrincipalId): Option[Int] =
    Try(UUID.fromString(PrincipalId.value(id))).toOption.map(_.version)

  /** Who `account` is now (`grit.links`): its person, whether by a vouching, and its home. */
  private def identityOf(account: Account): Option[(String, Boolean, String)] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT principal_id, vouched, home FROM grit.links WHERE account = ?"
        )
      ) { ps =>
        ps.setString(1, Account.written(account))
        Using.resource(ps.executeQuery()) { rs =>
          Option.when(rs.next())((rs.getString(1), rs.getBoolean(2), rs.getString(3)))
        }
      }
    }

  /** Each row `sql` reads with `params`, as its columns' text. */
  private def rows(sql: String, params: String*): Vector[Vector[String]] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        params.zipWithIndex.foreach((p, i) => ps.setString(i + 1, p))
        Using.resource(ps.executeQuery()) { rs =>
          val width = rs.getMetaData.getColumnCount
          val out = Vector.newBuilder[Vector[String]]
          while (rs.next()) out += (1 to width).map(rs.getString).toVector
          out.result()
        }
      }
    }

  /** The person of `email`, minted for it as a first attestation of it mints one. */
  private def emailPerson(email: String): String =
    rows(
      """WITH person AS (INSERT INTO grit.principals (id, kind)
        |                VALUES (uuidv7()::text, 'person') RETURNING id)
        |INSERT INTO grit.emails (email, principal_id) SELECT ?, id FROM person
        |RETURNING principal_id""".stripMargin,
      email
    ).flatten.headOption.getOrElse(sys.error(s"no person minted for $email"))

  /** `account` kept, and attested `email` (none for `None`) and `member`, `ago` before now. */
  private def attest(
      account: Account,
      email: Option[String],
      member: Boolean,
      ago: String
  ): Unit = {
    LiveDb
      .transaction(config)(SqlIdentities.enroll(Set(account)))
      .fold(e => sys.error(s"$e"), identity)
    execute(
      """INSERT INTO grit.attestations (account, email, member, seen_at)
        |VALUES (?, NULLIF(?, ''), ?::boolean, now() - ?::interval)""".stripMargin,
      Account.written(account),
      email.getOrElse(""),
      member.toString,
      ago
    )
  }

  /** `account`'s home. */
  private def homeOf(account: Account): String =
    rows(
      "SELECT home FROM grit.identities WHERE account = ?",
      Account.written(account)
    ).flatten.headOption
      .getOrElse(sys.error(s"${Account.written(account)} was never seen"))

  /** The accounts `person` holds now, in their spelling's order. */
  private def accountsOf(person: String): Vector[String] =
    rows("SELECT account FROM grit.links WHERE principal_id = ? ORDER BY account", person).flatten

  /** Postgres's plan for `sql` with `params`, every line, with sequential scans off: so the plan
    * shows which indexes can serve the read, whatever the test tables' sizes.
    */
  private def plan(sql: String, params: String*): String =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement("SET LOCAL enable_seqscan = off"))(ps => {
        val _ = ps.execute()
      })
      Using.resource(conn.prepareStatement(s"EXPLAIN $sql")) { ps =>
        params.zipWithIndex.foreach((p, i) => ps.setString(i + 1, p))
        Using.resource(ps.executeQuery()) { rs =>
          val out = Vector.newBuilder[String]
          while (rs.next()) out += rs.getString(1)
          out.result().mkString("\n")
        }
      }
    }

  /** The transaction id that wrote `account`'s row as it stands. */
  private def written(account: Account): String =
    rows(
      "SELECT xmin::text FROM grit.identities WHERE account = ?",
      Account.written(account)
    ).flatten.headOption
      .getOrElse(sys.error(s"${Account.written(account)} was never seen"))

  /** The SQLSTATE `sql` fails with; `None` when it succeeds. */
  private def refusal(sql: String, params: String*): Option[String] =
    Try(execute(sql, params*)).failed.toOption.flatMap {
      case e: java.sql.SQLException => Some(e.getSQLState)
      case _ => None
    }

  /** How many people `grit.principals` holds. */
  private def people(): Long =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement("SELECT count(*) FROM grit.principals WHERE kind = 'person'")
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          rs.next()
          rs.getLong(1)
        }
      }
    }

  /** Runs `sql` with `params`, arranging rows the stores would never write. */
  private def execute(sql: String, params: String*): Unit =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        params.zipWithIndex.foreach((p, i) => ps.setString(i + 1, p))
        val _ = ps.executeUpdate()
      }
    }

  /** A stored account in no account's spelling, and why reading it back fails. */
  private val Misspelt = "Not An Account"
  private val misspelt: StoreError = StoreError.Invalid(
    s"a stored account: an account is local, grit, or {namespace}:{name}: $Misspelt"
  )

  val tests = Tests {
    test(
      "an account first seen is a new person's one account, enrolled, under an id minted for them, never its spelling"
    ) {
      val engine = LiveEngine.open(config, "test")
      try {
        def said(thread: String, by: Account): Option[PrincipalId] =
          ingest(engine, thread, by).toOption.flatMap(author(engine, _, thread))
        val hers = said("1.0", ana)
        hers.flatMap(uuidVersion) ==> Some(7)
        said("2.0", ana) ==> hers
        val his = said("3.0", ben)
        assert(his.flatMap(uuidVersion) == Some(7), his != hers)
        identityOf(ana) ==> hers.map(p => (PrincipalId.value(p), false, PrincipalId.value(p)))
      } finally engine.close()
    }

    test("first sightings of one account at once make one person, and every one records") {
      val engine = LiveEngine.open(config, "test")
      val pool = Executors.newFixedThreadPool(8)
      given ExecutionContext = ExecutionContext.fromExecutorService(pool)
      try {
        val before = people()
        val accounts = (1 to 5).map(n => TestAccounts.account(s"slack:T1/URACE$n")).toVector
        val authors = accounts.map { account =>
          val go = new CountDownLatch(1)
          val threads = (1 to 8).map(n => s"race-${Account.written(account)}-$n").toVector
          val pending = Future.sequence(threads.map { thread =>
            Future {
              go.await()
              ingest(engine, thread, account).map(author(engine, _, thread))
            }
          })
          go.countDown()
          Await.result(pending, 60.seconds)
        }
        authors.flatten.collect { case Left(e) => e } ==> Vector()
        authors.map(_.distinct.size) ==> accounts.map(_ => 1)
        people() - before ==> accounts.size.toLong
      } finally {
        pool.shutdownNow()
        engine.close()
      }
    }

    test(
      "transactions enrolling the same unseen accounts at once, each naming them in another order, all record"
    ) {
      val pool = Executors.newFixedThreadPool(32)
      given ExecutionContext = ExecutionContext.fromExecutorService(pool)
      try {
        val before = people()
        val pairs = (1 to 16).map { n =>
          (TestAccounts.account(s"slack:T1/UPAIR$n-A"), TestAccounts.account(s"slack:T1/UPAIR$n-B"))
        }.toVector
        val go = new CountDownLatch(1)
        // A two-account set iterates in the order it was built, so each pair is named both ways.
        val enrolling = Future.sequence(pairs.flatMap { (a, b) =>
          Vector(Set(a, b), Set(b, a)).map { accounts =>
            Future {
              go.await()
              LiveDb.transaction(config)(SqlIdentities.enroll(accounts))
            }
          }
        })
        go.countDown()
        Await.result(enrolling, 60.seconds).collect { case Left(e) => e } ==> Vector()
        people() - before ==> 2L * pairs.size
      } finally pool.shutdownNow()
    }

    test(
      "an account stored in no account's spelling is Invalid where it is read back: as a conversation's creator, and among its asker's accounts"
    ) {
      val asker = TestAccounts.account("slack:T1/U-misspelt")
      val c = LiveDb.conversation(config, Origin.Slack("T1", "C1", "9.0")).id
      val turn = TurnRef(c, TurnSeq.First)
      LiveDb.asking(config, turn, asker, None)
      execute(
        """WITH home AS (INSERT INTO grit.principals (id, kind)
          |              VALUES (uuidv7()::text, 'person') RETURNING id)
          |INSERT INTO grit.identities (account, home) SELECT ?, id FROM home""".stripMargin,
        Misspelt
      )
      // The misspelt account and the asker are one person: both attested one email.
      val _ = emailPerson("misspelt@example.com")
      Vector(Account.written(asker), Misspelt).foreach(a =>
        execute(
          """INSERT INTO grit.attestations (account, email, member, seen_at)
            |VALUES (?, 'misspelt@example.com', false, now())""".stripMargin,
          a
        )
      )
      execute(
        "UPDATE grit.conversations SET created_by = ? WHERE id = ?::uuid",
        Misspelt,
        ConversationId.value(c)
      )
      LiveDb.transaction(config)(new SqlConversationStore().get(c)) ==> Left(misspelt)
      LiveDb.transaction(config)(LiveDb.Trialled.clearance(Subject.Turn(turn))) ==> Left(misspelt)
    }

    test("a review's rater stored in no account's spelling is Invalid where it is read back") {
      val c = LiveDb.conversation(config, Origin.Slack("T1", "C1", "9.1")).id
      execute(
        """INSERT INTO grit.reviews (entry_id, conversation_id, shadow, reason, considered_at,
          |       picked_at, address, posted_at, verdict, rater, labelled_at)
          |VALUES ('rated', ?::uuid, 'shadow', 'both', now(), now(), 'C1/9.1', now(), 'welcome', ?,
          |       now())""".stripMargin,
        ConversationId.value(c),
        Misspelt
      )
      LiveDb.transaction(config)(new SqlReviews().reviewed(Instant.EPOCH)) ==> Left(misspelt)
    }

    test(
      "a heard turn's asked account stored in no account's spelling is Invalid where it is read back"
    ) {
      val c = LiveDb.conversation(config, Origin.Slack("T1", "C1", "9.2")).id
      val turn = TurnRef(c, TurnSeq.First)
      LiveDb.asking(config, turn, ana, None)
      execute(
        """INSERT INTO grit.heard (entry_id, conversation_id, turn_seq, asked)
          |VALUES (?, ?::uuid, ?::bigint, ?::jsonb)""".stripMargin,
        s"${ConversationId.value(c)}:asked",
        ConversationId.value(c),
        TurnSeq.value(TurnSeq.First).toString,
        ujson.Arr(Account.written(ana), Misspelt).render()
      )
      LiveDb.transaction(config)(new SqlSpeechStore().reach(turn)) ==> Left(misspelt)
    }

    test(
      "an account a realm attests a claimed email is that email's person, by a vouching, a member as attested"
    ) {
      val account = TestAccounts.account("slack:T1/U-attested")
      val person = emailPerson("attested@example.com")
      attest(account, Some("attested@example.com"), member = true, "0 seconds")
      rows(
        "SELECT principal_id, vouched, member FROM grit.links WHERE account = ?",
        Account.written(account)
      ) ==>
        Vector(Vector(person, "t", "t"))
    }

    test("an attestation a year old still makes its account the email's person: no age ends it") {
      val account = TestAccounts.account("slack:T1/U-old")
      val person = emailPerson("old@example.com")
      attest(account, Some("old@example.com"), member = false, "1 year")
      identityOf(account) ==> Some((person, true, homeOf(account)))
    }

    test("an account attested no email is its home, a member as attested") {
      val account = TestAccounts.account("slack:T1/U-noemail")
      attest(account, None, member = true, "0 seconds")
      val home = homeOf(account)
      rows(
        "SELECT principal_id, vouched, member FROM grit.links WHERE account = ?",
        Account.written(account)
      ) ==>
        Vector(Vector(home, "f", "t"))
    }

    test(
      "two accounts attested one email are one person, read by its id; a home holds its one account"
    ) {
      val (a, b, c) = (
        TestAccounts.account("slack:T1/U-one-a"),
        TestAccounts.account("slack:T2/U-one-b"),
        TestAccounts.account("slack:T1/U-one-c")
      )
      val person = emailPerson("one@example.com")
      attest(a, Some("one@example.com"), member = true, "0 seconds")
      attest(b, Some("one@example.com"), member = false, "0 seconds")
      attest(c, None, member = true, "0 seconds")
      (accountsOf(person), accountsOf(homeOf(c)), accountsOf(homeOf(a))) ==> (
        Vector(Account.written(a), Account.written(b)),
        Vector(Account.written(c)),
        Vector()
      )
    }

    test(
      "a person's accounts are read through an index in each arm of grit.links, never a scan or a COALESCE"
    ) {
      val shown = plan(
        "SELECT jsonb_agg(jsonb_build_array(n.account, n.vouched, n.member)) FROM grit.links n WHERE n.principal_id = ?",
        "someone"
      )
      (
        shown.contains("Index Cond: (home = 'someone'::text)"),
        shown.contains("Index Cond: (principal_id = 'someone'::text)"),
        shown.contains("Seq Scan"),
        shown.contains("COALESCE")
      ) ==> (true, true, false, false)
    }

    test(
      "a realm's accounts read by prefix are bounded by the prefix index, whatever the collation"
    ) {
      plan(
        "SELECT account FROM grit.identities WHERE account LIKE ? ESCAPE '\\'",
        "slack:T1/%"
      ).contains(
        "Index Cond: ((account ~>=~ 'slack:T1/'::text) AND (account ~<~ 'slack:T10'::text))"
      ) ==> true
    }

    test(
      "naming an account by the name it already goes by writes nothing; a new name writes its row"
    ) {
      val account = TestAccounts.account("slack:T1/U-named")
      def name(n: String) =
        LiveDb
          .transaction(config)(new SqlPrincipals().name(account, n))
          .fold(e => sys.error(s"$e"), identity)
      name("Ana")
      val first = written(account)
      name("Ana")
      val same = written(account)
      name("Ana B")
      (same == first, written(account) == first) ==> (true, false)
    }

    test(
      "a second account at one home, and a second person for one email or email for one person, are refused"
    ) {
      val held = TestAccounts.account("slack:T1/U-home-held")
      LiveDb
        .transaction(config)(SqlIdentities.enroll(Set(held)))
        .fold(e => sys.error(s"$e"), identity)
      val person = emailPerson("keyed@example.com")
      (
        refusal(
          "INSERT INTO grit.identities (account, home) VALUES ('slack:T1/U-home-second', ?)",
          homeOf(held)
        ),
        refusal(
          """WITH p AS (INSERT INTO grit.principals (id, kind) VALUES (uuidv7()::text, 'person')
            |           RETURNING id)
            |INSERT INTO grit.emails (email, principal_id) SELECT 'keyed@example.com', id FROM p""".stripMargin
        ),
        refusal(
          "INSERT INTO grit.emails (email, principal_id) VALUES ('other@example.com', ?)",
          person
        )
      ) ==> (Some("23505"), Some("23505"), Some("23505"))
    }
  }
}
