package grit.dbos.engine

import java.util.concurrent.atomic.AtomicBoolean

import scala.annotation.unused
import scala.concurrent.duration.*
import scala.util.Using

import grit.core.durable.Durable
import grit.core.id.{PrincipalId, SourceId, TurnRef, WorkflowId}
import grit.core.inbox.Inbox
import grit.core.message.Message
import grit.core.store.{Origin, Tx}
import grit.dbos.sql.{
  DbConfig,
  LiveDb,
  SqlConversationStore,
  SqlEntryStore,
  SqlPeriodStore,
  TestPostgres
}

import dev.dbos.transact.DBOSClient
import org.postgresql.ds.PGSimpleDataSource
import utest.*

/** The engine lock (ADR 0015) against a real Postgres: one engine per database, a holder
  * named to whoever is refused, and an engine that stops when its lock is gone.
  */
object EngineLockTests extends TestSuite {

  /** A beat short enough for a test to watch several. */
  private val beat = 200.millis

  /** `config` with its schema applied and no engine running, as a first start leaves it. */
  private def fresh(name: String): DbConfig = {
    val c = TestPostgres.freshDatabase(name)
    LiveEngine.open(c, "test").close()
    c
  }

  /** The engine of `config`, beating every [[beat]]. */
  private def engine(config: DbConfig): Engine^ =
    EngineLock.take(config, beat) match {
      case Right(lock) => Engine.start(config, lock, "test")
      case Left(refused) => sys.error(s"not taken: $refused")
    }

  /** Runs `sql` on `config`'s database; the rows it changed. */
  private def run(config: DbConfig, sql: String): Int =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.createStatement())(_.executeUpdate(sql))
    }

  /** The one value `sql` reads from `config`'s database. */
  private def read(config: DbConfig, sql: String): Option[String] =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(conn.createStatement()) { st =>
        Using.resource(st.executeQuery(sql))(rs => Option.when(rs.next())(rs.getString(1)))
      }
    }

  /** Whether `config`'s lock is free: taken and at once released. */
  private def free(config: DbConfig): Boolean =
    EngineLock.take(config) match {
      case Right(lock) => lock.close(); true
      case Left(_) => false
    }

  /** Waits up to `within` for `ok`; whether it came. */
  private def eventually(within: FiniteDuration)(ok: => Boolean): Boolean = {
    val until = System.nanoTime() + within.toNanos
    var done = ok
    while (!done && System.nanoTime() < until) { Thread.sleep(50); done = ok }
    done
  }

  /** Terminates the server session holding `config`'s engine row: its lock goes with it. */
  private def terminateHolder(config: DbConfig): Unit = {
    val _ = read(config, "SELECT pg_terminate_backend(backend_pid)::text FROM grit.engines")
  }

  private def noop(id: WorkflowId)(using @unused d: Durable^): String = WorkflowId.value(id)

  private def started(inbox: Inbox^, source: String): TurnRef =
    (for {
      t <- inbox.ingest(
        Origin.Task("lock", source),
        SourceId(source),
        Message.User(source),
        PrincipalId.Local
      )
      _ <- inbox.startTurn(t)
    } yield t).fold(e => sys.error(s"inbox: $e"), identity)

  private def status(config: DbConfig, turn: TurnRef): Option[String] =
    read(
      config,
      s"SELECT status FROM dbos.workflow_status WHERE workflow_uuid = '${WorkflowId.value(turn.workflowId)}'"
    )

  val tests = Tests {
    test("a second engine on one database is refused, naming the first's machine and pid") {
      val config = fresh("lock_second")
      val first = engine(config)
      try
        EngineLock.take(config).left.map {
          case NotTaken.Held(holder) => holder.map(h => (h.machine, h.pid, h.epoch))
          case other => other
        } ==> Left(Some((EngineLock.machine, ProcessHandle.current().pid(), "test")))
      finally first.close()
    }

    test("a row left by a dead engine is not named as the holder") {
      val config = fresh("lock_dead_row")
      val dead = engine(config)
      try {
        terminateHolder(config)
        assert(eventually(5.seconds)(free(config)))
      } finally dead.close()
      // The dead engine's row is still there; the next holder has not written its own.
      val next = EngineLock.take(config) match {
        case Right(lock) => lock
        case Left(refused) => sys.error(s"$refused")
      }
      try EngineLock.take(config) ==> Left(NotTaken.Held(None))
      finally next.close()
    }

    test("two databases on one server do not contend") {
      val one = engine(fresh("lock_one"))
      try {
        val two = EngineLock.take(fresh("lock_two"))
        two.foreach(_.close())
        assert(two.isRight)
      } finally one.close()
    }

    test("a holder's heartbeat advances") {
      val config = fresh("lock_beat")
      val held = engine(config)
      try {
        val before = read(config, "SELECT heartbeat_at::text FROM grit.engines")
        assert(
          eventually(5.seconds)(
            read(config, "SELECT heartbeat_at::text FROM grit.engines") != before
          )
        )
      } finally held.close()
    }

    test(
      "an engine whose lock connection is terminated stops the turn it is running, which stays pending"
    ) {
      val config = fresh("lock_lost_running")
      val interrupted = new AtomicBoolean(false)
      val running = new AtomicBoolean(false)
      val lost = engine(config)
      try {
        lost.launch(
          _ =>
            d ?=> {
              d.step("block") { () =>
                running.set(true)
                try { Thread.sleep(60000); "slept" }
                catch { case _: InterruptedException => interrupted.set(true); "interrupted" }
              }
            },
          noop,
          noop,
          noop,
          Vector.empty
        )
        val turn = started(lost.inbox, "running")
        assert(eventually(20.seconds)(running.get()))
        terminateHolder(config)
        assert(eventually(beat * 3 + 2.seconds)(interrupted.get()))
        assert(eventually(5.seconds)(free(config)))
        status(config, turn) ==> Some("PENDING")
      } finally lost.close()
    }

    test("an engine whose lock is lost dequeues nothing after") {
      val config = fresh("lock_lost_queue")
      val lost = engine(config)
      val ran = new AtomicBoolean(false)
      try {
        lost.launch(
          id => d ?=> { ran.set(true); WorkflowId.value(id) },
          noop,
          noop,
          noop,
          Vector.empty
        )
        terminateHolder(config)
        assert(eventually(5.seconds)(free(config)))
        // Enqueued by an edge after the engine stopped: nothing here may run it.
        val ds = new PGSimpleDataSource()
        ds.setURL(config.jdbcUrl)
        ds.setUser(config.user)
        ds.setPassword(config.password)
        val client = new DBOSClient(ds)
        val turn =
          try {
            val entries = new SqlEntryStore()
            started(
              new SqlInbox(
                ds,
                client,
                new SqlConversationStore(),
                entries,
                new SqlPeriodStore(entries)
              ),
              "after"
            )
          } finally client.close()
        Thread.sleep(2000)
        (ran.get(), status(config, turn)) ==> (false, Some("ENQUEUED"))
      } finally lost.close()
    }

    test("closing an engine releases its lock only after its running bodies return") {
      val config = fresh("lock_close_order")
      val running = new AtomicBoolean(false)
      val interrupted = new AtomicBoolean(false)
      val closing = engine(config)
      closing.launch(
        _ =>
          d ?=> {
            d.step("stubborn") { () =>
              running.set(true)
              // Holds on for three seconds past its interrupt, as a step that ignores it would.
              try Thread.sleep(60000)
              catch { case _: InterruptedException => interrupted.set(true) }
              val until = System.nanoTime() + 3.seconds.toNanos
              while (System.nanoTime() < until) {}
              "done"
            }
          },
        noop,
        noop,
        noop,
        Vector.empty
      )
      val _ = started(closing.inbox, "stubborn")
      assert(eventually(20.seconds)(running.get()))
      val closer = new Thread(() => closing.close())
      closer.start()
      // DBOS has stopped and interrupted the body, which is still running.
      assert(eventually(20.seconds)(interrupted.get()))
      Thread.sleep(500)
      val during = free(config)
      closer.join()
      (during, free(config)) ==> (false, true)
    }

    test("an engine whose row is deleted stops, and frees the lock") {
      val config = fresh("lock_row_deleted")
      val held = engine(config)
      try {
        run(config, "DELETE FROM grit.engines") ==> 1
        assert(eventually(5.seconds)(free(config)))
      } finally held.close()
    }
  }
}
