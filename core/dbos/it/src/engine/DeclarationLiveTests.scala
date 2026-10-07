package grit.dbos.engine

import java.sql.DriverManager
import java.time.Instant

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Using

import grit.core.clock.SetClock
import grit.core.id.{
  ConversationId,
  EntryId,
  PluginName,
  PrincipalId,
  ScheduleId,
  TestCallSlots,
  TurnRef,
  TurnSeq
}
import grit.core.identity.{Account, DeclaredPerson, Handle, Identities, TestAccounts}
import grit.core.job.JobTests.{Count, Counting}
import grit.core.job.ScheduleContract.{booking, hour}
import grit.core.job.{DeskRefusal, ScheduleContract, ScheduleDesk, When}
import grit.core.store.{Origin, StoreError, Tx}
import grit.core.visibility.{Clearance, Label, Subject, TestLabels}
import grit.dbos.sql.{DbConfig, LiveDb, Opener, SqlLinks, SqlRoomReads, TestPostgres}

import utest.*

/** The people a deployment declares, made so at every engine start, against a real Postgres. */
object DeclarationLiveTests extends TestSuite {

  private val ana = TestAccounts.account("slack:T1/U-ana")
  private val anaMail = TestAccounts.account("mail:ana")
  private val ben = TestAccounts.account("slack:T1/U-ben")

  private def handle(name: String): Handle =
    Handle.of(name).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The deployment declaring each of `people`, by handle, with its accounts. */
  private def declaring(people: (String, Set[Account])*): Identities =
    Identities
      .of(people.toVector.map((h, accounts) => DeclaredPerson(handle(h), accounts)), Vector.empty)
      .fold(e => throw new java.lang.AssertionError(e.message), identity)

  /** An engine started on `config`'s database under `identities`, closed again. */
  private def started(config: DbConfig, identities: Identities): Unit =
    LiveEngine
      .open(config, "test", visibility = TestLabels.Trialled, identities = identities)
      .close()

  /** A fresh database an engine has started on, declaring no one. */
  private def fresh(suite: String): DbConfig = {
    val c = TestPostgres.freshDatabase(suite)
    started(c, Identities.Shipped)
    c
  }

  /** Rows `sql` reads, each as its columns' text. */
  private def rows(config: DbConfig, sql: String, params: String*): Vector[Vector[Option[String]]] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        params.zipWithIndex.foreach((p, i) => ps.setString(i + 1, p))
        Using.resource(ps.executeQuery()) { rs =>
          val width = rs.getMetaData.getColumnCount
          val out = Vector.newBuilder[Vector[Option[String]]]
          while (rs.next()) out += (1 to width).map(i => Option(rs.getString(i))).toVector
          out.result()
        }
      }
    }

  /** Runs `sql` with `params`, arranging rows the stores would never write. */
  private def execute(config: DbConfig, sql: String, params: String*): Unit =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.prepareStatement(sql)) { ps =>
        params.zipWithIndex.foreach((p, i) => ps.setString(i + 1, p))
        val _ = ps.executeUpdate()
      }
    }

  /** `account`'s row: its principal, evidence and home. */
  private def link(config: DbConfig, account: Account): Vector[Vector[Option[String]]] =
    rows(
      config,
      "SELECT principal_id, evidence, home FROM grit.identities WHERE account = ?",
      Account.written(account)
    )

  /** The principal declared as `name`. */
  private def declared(config: DbConfig, name: String): Option[String] =
    rows(config, "SELECT id FROM grit.principals WHERE handle = ?", name).flatten.headOption.flatten

  /** Every principal and every account's link, as stored. */
  private def people(config: DbConfig): Vector[Vector[Option[String]]] =
    rows(
      config,
      """SELECT p.id, p.kind, p.handle, i.account, i.evidence, i.home, i.linked_at::text
        |  FROM grit.principals p LEFT JOIN grit.identities i ON i.principal_id = p.id
        | ORDER BY p.id, i.account""".stripMargin
    )

  private val remind = new Counting("remind")

  private def desk(u: ScheduleContract.Under): ScheduleDesk^ =
    u.desk(
      PluginName.of("reminders").fold(sys.error, identity),
      Vector(remind.name),
      new SetClock(Instant.parse("2026-10-07T09:00:00Z"))
    )

  val tests = Tests {
    test(
      "a turn asked through one account is cleared by a group naming its person's other account, until the declaration drops it"
    ) {
      val config = fresh("declaration_cleared")
      val origin = Origin.Slack("T1", "C1", "1.0")
      val turn = TurnRef(LiveDb.conversation(config, origin, TestLabels.Trial).id, TurnSeq.First)
      LiveDb.asking(config, turn, ana, None)
      def resolved() =
        LiveDb.transaction(config)(new Opener(TestLabels.Trialled).clearance(Subject.Turn(turn)))
      val alone = resolved()
      started(config, declaring("ana" -> Set(ana, TestLabels.Trialist)))
      val joined = resolved()
      started(config, declaring("ana" -> Set(ana)))
      val dropped = resolved()
      def at(asker: Label) = Right(Clearance.inRoom(origin.room, TestLabels.Trial, asker))
      (alone, joined, dropped) ==> (at(Label.Public), at(TestLabels.Trial), at(Label.Public))
    }

    test("a start under the declaration the database already holds changes nothing") {
      val config = fresh("declaration_again")
      LiveDb.asking(
        config,
        TurnRef(LiveDb.conversation(config, Origin.Slack("T1", "C1", "2.0")).id, TurnSeq.First),
        ana,
        None
      )
      val identities = declaring("ana" -> Set(ana, anaMail), "ben" -> Set(ben))
      started(config, identities)
      val once = people(config)
      started(config, identities)
      val twice = people(config)
      assert(declared(config, "ana").isDefined, declared(config, "ben").isDefined)
      twice ==> once
    }

    test(
      "an account dropped from a declaration goes back to a new person of its own, enrolled, and the rest stay declared"
    ) {
      val config = fresh("declaration_dropped")
      started(config, declaring("ana" -> Set(ana, anaMail)))
      val person = declared(config, "ana")
      started(config, declaring("ana" -> Set(ana)))
      val home = link(config, anaMail).flatten.headOption.flatten
      (link(config, ana), link(config, anaMail), home == person) ==> (
        Vector(Vector(person, Some("declared"), None)),
        Vector(Vector(home, Some("enrolled"), home)),
        false
      )
    }

    test(
      "a handle no longer declared is cleared, and its accounts go back to people of their own"
    ) {
      val config = fresh("declaration_undeclared")
      started(config, declaring("ana" -> Set(ana), "ben" -> Set(ben)))
      started(config, declaring("ben" -> Set(ben)))
      val home = link(config, ana).flatten.headOption.flatten
      (declared(config, "ana"), link(config, ana), home.isDefined) ==> (
        None,
        Vector(Vector(home, Some("enrolled"), home)),
        true
      )
    }

    test(
      "two people declared as one become one: their emptied homes are deleted, and the schedules, edges and tool requests naming them are the declared person's"
    ) {
      val (u, config) = SqlSchedulesUnder.withConfig("declaration_merge")
      val thread = u.turn(Origin.Slack("T1", "C1", "3.0"))
      u.asking(thread, ana, Some("C1/3.0"))
      u.asking(u.turn(Origin.Slack("T1", "C1", "3.1")), ben, Some("C1/3.1"))
      val asked = desk(u)
        .ask(TestCallSlots.at(thread), booking(remind), When.In(1.hour), hour, Count(1))
        .fold(r => sys.error(s"$r"), _.id)
      val (hers, his) = (PrincipalId.value(u.principal(ana)), PrincipalId.value(u.principal(ben)))
      val room =
        rows(config, "SELECT room_id FROM grit.schedules WHERE id = ?", ScheduleId.value(asked))
      execute(
        config,
        """INSERT INTO grit.edges (key, principal, machine, pid, session, protocol, started_at,
          |                        heartbeat_at)
          |VALUES ('planted', ?, 'machine', 1, uuidv7(), 1, now(), now())""".stripMargin,
        his
      )
      execute(
        config,
        """INSERT INTO grit.tool_requests (key, protocol, workflow_id, conversation_id, turn_seq,
          |       workspace_id, principal, tool, permit, retry, arguments, repairs, state)
          |SELECT 'planted', 1, 'planted', c.id, 1, c.place_id, ?, 'read', 'free', 'rerun',
          |       '{}'::jsonb, '[]'::jsonb, 'open'
          |  FROM grit.conversations c WHERE c.id = ?::uuid""".stripMargin,
        his,
        ConversationId.value(thread.conversationId)
      )
      started(config, declaring("ana" -> Set(ana, ben)))
      val person = declared(config, "ana")
      (
        rows(config, "SELECT count(*) FROM grit.principals WHERE id IN (?, ?)", hers, his),
        rows(
          config,
          "SELECT principal, room_id FROM grit.schedules WHERE id = ?",
          ScheduleId.value(asked)
        ),
        rows(config, "SELECT principal FROM grit.edges WHERE key = 'planted'"),
        rows(config, "SELECT principal FROM grit.tool_requests WHERE key = 'planted'")
      ) ==> (
        Vector(Vector(Some("0"))),
        Vector(Vector(person, room.flatten.headOption.flatten)),
        Vector(Vector(person)),
        Vector(Vector(person))
      )
    }

    test("two accounts of one person share the pending cap") {
      val (u, config) = SqlSchedulesUnder.withConfig("declaration_cap")
      val hers = u.turn("4.0")
      val his = u.turn("4.1")
      u.asking(hers, ana, Some("C1/4.0"))
      u.asking(his, ben, Some("C1/4.1"))
      started(config, declaring("ana" -> Set(ana, ben)))
      val d = desk(u)
      def ask(from: TurnRef, index: Int) =
        d.ask(
          TestCallSlots.at(from, index = index),
          booking(remind),
          When.In(1.hour),
          hour,
          Count(index)
        )
      (0 until ScheduleDesk.PendingCap).map(ask(hers, _)).collect { case Left(r) => r } ==> Vector()
      ask(his, 100) ==> Left(DeskRefusal.TooMany(ScheduleDesk.PendingCap))
    }

    test("a person's messages are those written through any of their accounts") {
      val config = fresh("declaration_said")
      val turns = Vector(ana -> "5.0", ben -> "5.1").map { (by, thread) =>
        val turn =
          TurnRef(LiveDb.conversation(config, Origin.Slack("T1", "C1", thread)).id, TurnSeq.First)
        LiveDb.asking(config, turn, by, None)
        turn
      }
      started(config, declaring("ana" -> Set(ana, ben)))
      val person = LiveDb.principal(config, ana)
      LiveDb
        .transaction(config)(
          new SqlRoomReads().saidBy(
            Origin.Slack("T1", "C1", "5.0").room,
            person,
            Instant.parse("2026-10-07T07:00:00Z"),
            Instant.parse("2026-10-07T09:00:00Z"),
            Set.empty,
            10
          )
        )
        .map(_.map(_.entry.id).toSet) ==> Right(
        turns.map(t => EntryId(s"${ConversationId.value(t.conversationId)}:asked")).toSet
      )
    }

    test(
      "a schedule asked through one account is listed through the other in its room, and not from a room that may not read it"
    ) {
      val (u, config) = SqlSchedulesUnder.withConfig("declaration_listed")
      val asked = u.turn(Origin.Slack("T1", "C9", "6.0"), TestLabels.Trial)
      val same = u.turn(Origin.Slack("T1", "C9", "6.1"), TestLabels.Trial)
      val lower = u.turn(Origin.Slack("T1", "C8", "6.2"))
      u.asking(asked, ana, Some("C9/6.0"))
      u.asking(same, ben, Some("C9/6.1"))
      u.asking(lower, ben, Some("C8/6.2"))
      started(config, declaring("ana" -> Set(ana, ben)))
      val d = desk(u)
      val id = d
        .ask(TestCallSlots.at(asked), booking(remind), When.In(1.hour), hour, Count(1))
        .fold(r => sys.error(s"$r"), _.id)
      def listed(from: TurnRef) =
        d.pending(TestCallSlots.at(from, index = 1), booking(remind)).map(_.schedules.map(_.id))
      (listed(same), listed(lower)) ==> (Right(Vector(id)), Right(Vector()))
    }

    test(
      "an ask beside a merge of its asker waits for it, then writes nothing and says to ask again; asked again, it is the declared person's"
    ) {
      val (u, config) = SqlSchedulesUnder.withConfig("declaration_beside")
      val thread = u.turn("8.0")
      u.asking(thread, ana, Some("C1/8.0"))
      val d = desk(u)
      def ask(index: Int) =
        d.ask(
          TestCallSlots.at(thread, index = index),
          booking(remind),
          When.In(1.hour),
          hour,
          Count(index)
        )
      given ExecutionContext = ExecutionContext.global
      // The merge's transaction holds the home's lock until an ask waits on it, then commits.
      val beside = LiveDb.transaction(config) {
        SqlLinks
          .reconcile(declaring("ana" -> Set(ana)))
          .fold(e => sys.error(s"merging: $e"), identity)
        val asking = Future(ask(1))
        waitingOnALock(config)
        asking
      }
      val refused = Await.result(beside, 30.seconds)
      val unwritten = rows(config, "SELECT count(*) FROM grit.schedules")
      val again = ask(2).map(_.id)
      val person = declared(config, "ana")
      (
        refused,
        unwritten,
        again.map(id =>
          rows(config, "SELECT principal FROM grit.schedules WHERE id = ?", ScheduleId.value(id))
        )
      ) ==> (
        Left(
          DeskRefusal.Unavailable(
            StoreError.Invalid("the asker was merged into a declared person; ask again")
          )
        ),
        Vector(Vector(Some("0"))),
        Right(Vector(Vector(person)))
      )
    }
  }

  /** Returns once a transaction on `config`'s database waits on a lock; throws after 10 s. */
  private def waitingOnALock(config: DbConfig): Unit =
    Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
      conn =>
        def waiting(): Boolean =
          Using.resource(
            conn.prepareStatement(
              """SELECT 1 FROM pg_stat_activity
                | WHERE datname = current_database() AND wait_event_type = 'Lock'""".stripMargin
            )
          )(ps => Using.resource(ps.executeQuery())(_.next()))
        // Every 20 ms, 500 times.
        val came = Iterator.range(0, 500).exists { _ =>
          waiting() || { Thread.sleep(20); false }
        }
        if (!came) sys.error("no transaction came to wait on a lock")
    }
}
