package grit.dbos.engine

import java.sql.{Connection, DriverManager}
import java.time.Instant
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Try, Using}

import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{
  ConversationId,
  PeriodRef,
  PeriodSeq,
  SourceId,
  TriageRef,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.identity.Account
import grit.core.inbox.InboxError
import grit.core.message.Message
import grit.core.speech.Reach
import grit.core.stitch.{Opening, Placements}
import grit.core.store.{Origin, Payload}
import grit.core.visibility.{Subject, Visibility}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}

import utest.*

/** What a rehearsal's session leaves behind on a database an engine runs on, and what it holds
  * while it runs: nothing written survives it, and what it locks is rows, held no longer than
  * its cap.
  */
object RehearsingLiveTests extends TestSuite {

  private val at = Instant.parse("2026-10-17T09:00:00Z")

  private val room = Origin.Slack("T1", "C1", "1.0")

  /** A database an engine has opened (its schema applied, nothing launched), holding one heard
    * thread at `room`, and that thread's conversation.
    */
  private def seeded(suite: String): (DbConfig, Engine^, ConversationId) = {
    val config = TestPostgres.freshDatabase(suite)
    val engine = LiveEngine.open(config, "test")
    engine.inbox.hear(room, SourceId("1.0"), "lunch?", Account.Local, at, Reach.Nowhere, None) ==>
      Right(())
    (config, engine, LiveDb.conversation(config, room).id)
  }

  /** One body per workflow, each a value of its own, so a runner can tell which it was handed. */
  private val bodies: Vector[WorkflowId -> Durable^ ?-> String] =
    Vector.tabulate(8)(i => id => (_: Durable^) ?=> s"body $i: ${WorkflowId.value(id)}")

  /** A runner that runs nothing: it notes, for each body it is handed, whether it is the one of
    * [[bodies]] in its place, and each turn handed to it, which it takes; it placed nothing.
    */
  private final class Handed extends Rehearsing.Runner {
    val handed = scala.collection.mutable.ArrayBuffer.empty[Boolean]
    val turns = scala.collection.mutable.ArrayBuffer.empty[TurnRef]
    def launched(
        session: Rehearsing^,
        turn: WorkflowId => Durable^ ?=> String,
        close: WorkflowId => Durable^ ?=> String,
        settle: WorkflowId => Durable^ ?=> String,
        post: WorkflowId => Durable^ ?=> String,
        triage: WorkflowId => Durable^ ?=> String,
        stitch: WorkflowId => Durable^ ?=> String,
        shadow: WorkflowId => Durable^ ?=> String,
        run: WorkflowId => Durable^ ?=> String
    ): Unit =
      handed ++= Vector(
        turn eq bodies(0),
        close eq bodies(1),
        settle eq bodies(2),
        post eq bodies(3),
        triage eq bodies(4),
        stitch eq bodies(5),
        shadow eq bodies(6),
        run eq bodies(7)
      )
    def started(turn: TurnRef): Either[InboxError, Unit] = {
      turns += turn
      Right(())
    }
    val placements: Placements = new Placements {
      def awaited(opening: Opening): Either[String, String] = Left("not placed")
      def awaitedWithin(
          opening: Opening,
          within: FiniteDuration,
          clock: Clock^
      ): Either[Placements.Unplaced, String] = Left(Placements.Unplaced.Failed("not placed"))
    }
  }

  private def connect(config: DbConfig): Connection =
    DriverManager.getConnection(config.jdbcUrl, config.user, config.password)

  /** Every table of the `grit` and `dbos` schemas, with its rows counted and an md5 of their
    * text in order, so a row rewritten in place shows as a change.
    */
  private def contents(config: DbConfig): Map[String, (Long, String)] =
    Using.resource(connect(config)) { c =>
      val tables = Using.resource(c.createStatement()) { s =>
        Using.resource(
          s.executeQuery(
            """SELECT table_schema || '.' || table_name FROM information_schema.tables
              | WHERE table_schema IN ('grit', 'dbos') AND table_type = 'BASE TABLE'""".stripMargin
          )
        ) { rs => Iterator.continually(rs).takeWhile(_.next()).map(_.getString(1)).toVector }
      }
      tables.map { t =>
        t -> Using.resource(c.createStatement()) { s =>
          Using.resource(
            s.executeQuery(
              s"SELECT count(*), coalesce(md5(string_agg(r::text, E'\\n' ORDER BY r::text)), '') FROM $t AS r"
            )
          ) { rs =>
            rs.next()
            (rs.getLong(1), rs.getString(2))
          }
        }
      }.toMap
    }

  /** The lock modes the rehearsal's session holds on relations, as another session sees them. */
  private def relationLocks(config: DbConfig): Set[String] =
    Using.resource(connect(config)) { c =>
      Using.resource(
        c.prepareStatement(
          """SELECT DISTINCT l.mode FROM pg_locks l JOIN pg_stat_activity a ON a.pid = l.pid
            | WHERE a.application_name = ? AND a.datname = current_database()
            |   AND l.locktype = 'relation'""".stripMargin
        )
      ) { ps =>
        ps.setString(1, Rehearsing.ApplicationName)
        Using.resource(ps.executeQuery()) { rs =>
          Iterator.continually(rs).takeWhile(_.next()).map(_.getString(1)).toSet
        }
      }
    }

  /** Whether another session can lock `conversation`'s row at once, without waiting. */
  private def lockable(config: DbConfig, conversation: ConversationId): Boolean =
    Using.resource(connect(config)) { c =>
      // NOWAIT does not cover the table's own lock: this bounds that wait too.
      Using.resource(c.createStatement())(_.execute("SET lock_timeout = 1000"))
      c.setAutoCommit(false)
      try
        Using.resource(
          c.prepareStatement(
            "SELECT 1 FROM grit.conversations WHERE id = ?::uuid FOR UPDATE NOWAIT"
          )
        ) { ps =>
          ps.setString(1, ConversationId.value(conversation))
          Try(Using.resource(ps.executeQuery())(_.next())).getOrElse(false)
        }
      finally c.rollback()
    }

  val tests = Tests {
    test(
      "a rehearsed hear and ingest leave every table as it was, though the session read what they wrote"
    ) {
      val (config, engine, thread) = seeded("rehearsing_nothing")
      try {
        val before = contents(config)
        val said = Origin.Slack("T1", "C1", "2.0")
        val shown =
          Rehearsing(config, LiveEngine.Uncapped, Visibility.Shipped, Clock.system(), new Handed) {
            r =>
              val reply =
                r.hear(room, SourceId("1.1"), "yes", Account.Local, at, Reach.Nowhere)
              val asked =
                r.ingest(said, SourceId("2.0"), Message.User("@grit lunch?"), Account.Local)
              val read = asked.toOption.map(s =>
                r.db
                  .read(Subject.Conversation(s.turn.conversationId))(
                    r.entries.list(s.turn.conversationId)
                  )
                  .map(_.map(_.payload))
              )
              (reply, asked.map(_.opening.isDefined), read)
          }
        shown ==> Right(
          (
            Right(
              Starts.Heard(None, Some(TriageRef(PeriodRef(thread, PeriodSeq.First), TurnSeq(1))))
            ),
            Right(true),
            Some(Right(Vector(Payload.Message(Message.User("@grit lunch?")))))
          )
        )
        contents(config) ==> before
      } finally engine.close()
    }

    test(
      "a session launched hands every body to its runner and leaves every table as it was; a turn started on it reaches the runner"
    ) {
      val (config, engine, thread) = seeded("rehearsing_runner")
      try {
        val before = contents(config)
        val runner = new Handed
        val turn = TurnRef(thread, TurnSeq(2))
        val shown =
          Rehearsing(config, LiveEngine.Uncapped, Visibility.Shipped, Clock.system(), runner) { r =>
            r.launch(
              bodies(0),
              bodies(1),
              bodies(2),
              bodies(3),
              bodies(4),
              bodies(5),
              Vector.empty,
              bodies(6),
              Vector.empty,
              bodies(7)
            )
            r.startTurn(turn)
          }
        (shown, runner.handed.toVector, runner.turns.toVector) ==> (
          Right(Right(())),
          Vector.fill(8)(true),
          Vector(turn)
        )
        contents(config) ==> before
      } finally engine.close()
    }

    test("a session's write waits no longer than LockWait for a row another session holds") {
      val (config, engine, thread) = seeded("rehearsing_wait")
      val holder = connect(config)
      try {
        holder.setAutoCommit(false)
        Using.resource(
          holder.prepareStatement("SELECT 1 FROM grit.conversations WHERE id = ?::uuid FOR UPDATE")
        ) { ps =>
          ps.setString(1, ConversationId.value(thread))
          Using.resource(ps.executeQuery())(_.next())
        }
        // Released once the session is done, or after a wait no session should make, so a
        // session that waits ends.
        val done = new CountDownLatch(1)
        val release = Future {
          done.await(4, TimeUnit.SECONDS)
          holder.rollback()
        }(using ExecutionContext.global)
        val started = System.nanoTime()
        val heard =
          Rehearsing(config, LiveEngine.Uncapped, Visibility.Shipped, Clock.system(), new Handed) {
            r => r.hear(room, SourceId("1.1"), "yes", Account.Local, at, Reach.Nowhere)
          }
        val waited = (System.nanoTime() - started).nanos
        done.countDown()
        Await.result(release, 10.seconds)
        assert(heard.toOption.exists(_.left.exists(_.isInstanceOf[InboxError.Unavailable])))
        assert(waited < Rehearsing.LockWait + 1.second)
      } finally {
        holder.close()
        engine.close()
      }
    }

    test(
      "while a session holds a hear, it holds no relation lock above a row's, and an engine hears into another conversation"
    ) {
      val (config, engine, _) = seeded("rehearsing_beside")
      try {
        val shown =
          Rehearsing(config, LiveEngine.Uncapped, Visibility.Shipped, Clock.system(), new Handed) {
            r =>
              val reply = r.hear(room, SourceId("1.1"), "yes", Account.Local, at, Reach.Nowhere)
              val elsewhere = Future(
                engine.inbox.hear(
                  Origin.Slack("T1", "C2", "5.0"),
                  SourceId("5.0"),
                  "hello",
                  Account.Local,
                  at,
                  Reach.Nowhere,
                  None
                )
              )(using ExecutionContext.global)
              (
                reply.isRight,
                relationLocks(config),
                Try(Await.result(elsewhere, 5.seconds)).toOption
              )
          }
        val (heard, modes, elsewhere) = shown.fold(e => sys.error(s"$e"), identity)
        heard ==> true
        assert(
          modes.nonEmpty,
          modes.subsetOf(Set("AccessShareLock", "RowShareLock", "RowExclusiveLock"))
        )
        elsewhere ==> Some(Right(()))
      } finally engine.close()
    }

    test("a session past its cap is ended there, its locks freed, and shows nothing") {
      val (config, engine, thread) = seeded("rehearsing_cap")
      try {
        val within = 500.millis
        var freed = false
        val shown =
          Rehearsing(
            config,
            LiveEngine.Uncapped,
            Visibility.Shipped,
            Clock.system(),
            new Handed,
            within
          ) { r =>
            val heard = r.hear(room, SourceId("1.1"), "yes", Account.Local, at, Reach.Nowhere)
            Thread.sleep(1000)
            freed = lockable(config, thread)
            heard.isRight
          }
        shown ==> Left(Rehearsing.Unrehearsed.Overran(within))
        freed ==> true
      } finally engine.close()
    }
  }
}
