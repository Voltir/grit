package grit.dbos.engine

import java.sql.Connection
import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{Executor, Executors, TimeUnit}

import scala.concurrent.duration.*
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{SourceId, TurnRef, WorkflowId}
import grit.core.identity.Account
import grit.core.inbox.InboxError
import grit.core.message.Message
import grit.core.plugin.Plugin
import grit.core.speech.Reach
import grit.core.spend.Budget
import grit.core.stitch.Placements
import grit.core.store.{Db, Jot, Origin, StoreError, Tx}
import grit.core.triage.Shadowing
import grit.core.visibility.{Subject, Visibility}
import grit.dbos.sql.{DbConfig, SqlEntryStore, SqlStatements}

import org.postgresql.ds.PGSimpleDataSource

/** The stores over one session on a database, beside any engine running on it, whose writes
  * are all undone when the session ends ([[Rehearsing.apply]]): every transaction it runs is a
  * savepoint of one transaction that is rolled back, so nothing it writes is seen by another
  * session or outlives it but the identity and sequence values its inserts drew. A row it
  * locks is held until the session ends. What it is launched with goes to its runner
  * ([[Rehearsing.Runner]]).
  */
final class Rehearsing private (
    conn: Connection,
    budget: Budget,
    visibility: Visibility,
    val clock: Clock^,
    runner: Rehearsing.Runner^
) extends Stores(visibility),
      Runtime {

  private val recorder = new Recorder(conversations, entries, periods, speech, spending, budget)

  /** Held while a savepoint is open: one at a time, whichever thread asks. */
  private val serial = new Object

  /** Reads, each rolled back as it returns. */
  val db: Db^{this} = new Db {
    def read[A](subject: Subject)(
        body: (Tx^) ?=> Either[StoreError, A]
    ): Either[StoreError, A] =
      savepoint(keep = false)(opener.open(subject, conn).flatMap(tx => body(using tx)))
  }

  /** Writes, each kept for the session's later reads when its body returns a `Right`, and
    * undone with the rest when the session ends.
    */
  val jot: Jot^{this} = new Jot {
    def write[A](subject: Subject)(
        body: (Tx^) ?=> Either[StoreError, A]
    ): Either[StoreError, A] =
      savepoint(keep = true)(opener.open(subject, conn).flatMap(tx => body(using tx)))
  }

  /** The changes people make, written through [[jot]], so undone with the session. */
  val administration: grit.core.admin.Administration =
    new grit.dbos.sql.SqlAdministration(jot, opener)

  /** Where each opening is placed: its runner's ([[Rehearsing.Runner.placements]]). */
  val placements: Placements^{this} = runner.placements

  /** Hands `turn` to its runner ([[Rehearsing.Runner.started]]), which runs it after the body
    * that started it; the runner's refusal, an [[InboxError.Invalid]], otherwise.
    */
  def startTurn(turn: TurnRef): Either[InboxError, Unit] = runner.started(turn)

  /** Hands every body to its runner ([[Rehearsing.Runner.launched]]), which runs the
    * situation's workflows through them, in this session, before this returns; `plugins` and
    * `shadowing` are unused, as nothing sweeps. Declares no document terms and registers no
    * workflow. Throws only what the runner lets escape, which is a fault of the runner.
    */
  override def launch(
      turn: WorkflowId => Durable^ ?=> String,
      close: WorkflowId => Durable^ ?=> String,
      settle: WorkflowId => Durable^ ?=> String,
      post: WorkflowId => Durable^ ?=> String,
      triage: WorkflowId => Durable^ ?=> String,
      stitch: WorkflowId => Durable^ ?=> String,
      plugins: Vector[Plugin],
      shadow: WorkflowId => Durable^ ?=> String,
      shadowing: Vector[Shadowing],
      run: WorkflowId => Durable^ ?=> String
  ): Unit =
    runner.launched(this, turn, close, settle, post, triage, stitch, shadow, run)

  /** `text` recorded as [[grit.core.inbox.Inbox.hear]] records it, with no link, and what that
    * starts; the inbox's refusals, and `Unavailable` when a statement fails.
    */
  def hear(
      origin: Origin,
      source: SourceId,
      text: String,
      by: Account,
      at: Instant,
      reach: Reach
  ): Either[InboxError, Starts.Heard] =
    SqlInbox
      .undirected(origin, "a direct message is never heard")
      .flatMap(_ =>
        maintained(recorder.heard(origin, source, text, by, at, reach, None)).left
          .map(InboxError.stored)
      )

  /** `message` recorded as [[grit.core.inbox.Inbox.ingest]] records it, with no link, dated now,
    * and what that starts; the inbox's refusals (over the day's cap among them), and
    * `Unavailable` when a statement fails.
    */
  def ingest(
      origin: Origin,
      source: SourceId,
      message: Message.User,
      by: Account
  ): Either[InboxError, Starts.Said] = {
    val now = clock.now()
    SqlInbox
      .writtenBy(origin, by)
      .flatMap(_ =>
        maintained(recorder.said(origin, source, message, by, now, None)).left
          .map(InboxError.stored)
          .flatMap(identity)
      )
  }

  /** `body` written in a savepoint at maintenance's clearance, as the inbox writes. */
  private def maintained[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    savepoint(keep = true)(opener.maintained(conn).flatMap(tx => body(using tx)))

  /** `run` in a savepoint of the session's transaction: released when `keep` and `run` returns
    * a `Right`, so what it wrote stays for the session's later statements, and rolled back
    * otherwise, a throw being a `Left`. A release that fails rolls back instead, so a failed
    * statement never leaves the session's transaction failed.
    */
  private def savepoint[A](keep: Boolean)(run: => Either[StoreError, A]): Either[StoreError, A] =
    serial.synchronized {
      SqlStatements.attempt(conn.setSavepoint()).flatMap { point =>
        val result =
          try run
          catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
        SqlStatements
          .attempt(
            if (keep && result.isRight) conn.releaseSavepoint(point) else conn.rollback(point)
          )
          .left
          .flatMap(why => SqlStatements.attempt(conn.rollback(point)).flatMap(_ => Left(why)))
          .flatMap(_ => result)
      }
    }
}

object Rehearsing {

  /** How long a statement waits for a row lock another session holds before it fails: 1 s. */
  val LockWait: FiniteDuration = 1.second

  /** How long one statement may run before it fails: 10 s. */
  val StatementWithin: FiniteDuration = 10.seconds

  /** How long a session may last: 60 s. The server also ends a session idle in its transaction
    * this long, should this process stall holding it.
    */
  val Within: FiniteDuration = 60.seconds

  /** The name a session gives the server, as `pg_stat_activity` shows it. */
  val ApplicationName: String = "grit rehearsal"

  /** What a session hands on to the rehearsal that opened it: the bodies it is launched with,
    * each turn they start, and where each opening is placed.
    */
  trait Runner extends caps.SharedCapability {

    /** Runs the workflows the rehearsal is of through these bodies, in `session`, before the
      * launch that hands them over returns. Every body is the shipped one the launch built. A
      * rehearsal's stop, at the turn's own model call, is caught here: nothing escapes for it.
      */
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
    ): Unit

    /** Takes `turn`, started by a body it is running, to run after that body;
      * [[InboxError.Invalid]] saying why when it will not.
      */
    def started(turn: TurnRef): Either[InboxError, Unit]

    /** What each opening's placement, as this runner ran it, returned; for one it did not
      * place, a `Left` saying so (a bounded wait's [[Placements.Unplaced.Failed]]), never a
      * wait.
      */
    val placements: Placements
  }

  /** Why a session showed nothing. */
  enum Unrehearsed {

    /** The database could not be opened, saying why. */
    case Unopened(why: String)

    /** The session outlasted `within`: it was ended there, its locks freed, and every
      * statement after failed.
      */
    case Overran(within: FiniteDuration)
  }

  /** `body` run over a session on the database `config` names, recording as an inbox under
    * `budget` and `visibility` records, dating by `clock`, handing what it is launched with to
    * `runner`; when `body` returns or throws, or
    * once `within` has passed, the session's transaction is rolled back and the session
    * closed. Opens no DBOS executor or client and takes no engine lock. Each statement reads
    * what is committed as it runs (READ COMMITTED), waits at most [[LockWait]] for a row lock,
    * and runs at most [[StatementWithin]].
    */
  private[grit] def apply[A](
      config: DbConfig,
      budget: Budget,
      visibility: Visibility,
      clock: Clock^,
      runner: Runner^,
      within: FiniteDuration = Within
  )(body: (r: Rehearsing^) => A): Either[Unrehearsed, A] =
    opened(config) match {
      case Left(why) => Left(Unrehearsed.Unopened(why))
      case Right(conn) =>
        val overran = new AtomicBoolean(false)
        val timer = Executors.newSingleThreadScheduledExecutor { run =>
          val t = new Thread(run, "grit.rehearsing")
          t.setDaemon(true)
          t
        }
        // JDBC's abort is the call another thread may make on a connection in use: it frees
        // the session's locks at once, and fails every later statement on it.
        val inline: Executor = run => run.run()
        timer.schedule(
          (() => {
            overran.set(true)
            conn.abort(inline)
          }): Runnable,
          within.toMillis,
          TimeUnit.MILLISECONDS
        )
        try {
          val result = body(new Rehearsing(conn, budget, visibility, clock, runner))
          timer.shutdownNow()
          timer.awaitTermination(1, TimeUnit.SECONDS)
          if (overran.get()) Left(Unrehearsed.Overran(within)) else Right(result)
        } finally {
          timer.shutdownNow()
          try conn.rollback()
          catch { case NonFatal(_) => () }
          try conn.close()
          catch { case NonFatal(_) => () }
        }
    }

  /** A connection to `config`'s database, its session's limits set and its transaction begun. */
  private def opened(config: DbConfig): Either[String, Connection] =
    try {
      val ds = new PGSimpleDataSource()
      ds.setURL(config.jdbcUrl)
      ds.setUser(config.user)
      ds.setPassword(config.password)
      ds.setApplicationName(ApplicationName)
      val conn = ds.getConnection()
      try {
        Using.resource(conn.createStatement()) { s =>
          s.execute(s"SET lock_timeout = ${LockWait.toMillis}")
          s.execute(s"SET statement_timeout = ${StatementWithin.toMillis}")
          s.execute(s"SET idle_in_transaction_session_timeout = ${Within.toMillis}")
        }
        conn.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED)
        conn.setAutoCommit(false)
        Right(conn)
      } catch {
        case NonFatal(e) =>
          conn.close()
          throw e
      }
    } catch {
      case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.toString))
    }
}
