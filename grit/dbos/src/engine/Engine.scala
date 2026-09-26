package grit.dbos.engine

import java.sql.DriverManager
import java.time.Instant
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scala.concurrent.duration.FiniteDuration
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.id.{ConversationId, TurnRef, WorkflowId}
import grit.core.inbox.Inbox
import grit.core.plugin.{Plugin, PluginCursors, PluginDocs, PluginName}
import grit.core.store.{
  ConversationStore,
  Db,
  EntrySearch,
  EntryStore,
  Jot,
  LifecycleStore,
  ModelFactStore,
  ModelProfileStore,
  Origin,
  PeriodStore,
  StoreError,
  Tx,
  UsageLedger
}
import grit.dbos.sql.{
  DbConfig,
  SqlConversationStore,
  SqlDb,
  SqlEntrySearch,
  SqlEntryStore,
  SqlJot,
  SqlLifecycleStore,
  SqlModelFactStore,
  SqlModelProfileStore,
  SqlPeriodStore,
  SqlPluginCursors,
  SqlPluginDocs,
  SqlUsageLedger
}
import grit.dbos.workflow.{Closes, Posts, Turns}

import dev.dbos.transact.config.DBOSConfig
import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.workflow.WorkflowState
import dev.dbos.transact.{DBOS, DBOSClient}
import org.postgresql.ds.PGSimpleDataSource
import org.slf4j.LoggerFactory

/** grit over one Postgres: the stores, the turn and close workflows, the sweep that closes
  * periods, and an edge's [[Inbox]], all in this process. Open it, [[launch]] it with the
  * turn's and the close's bodies, start its [[sweepEvery]], and close it when done; its
  * threads keep the JVM alive until then.
  */
final class Engine private (dbos: DBOS, dataSource: PGSimpleDataSource)
    extends caps.SharedCapability,
      AutoCloseable {

  val conversations: ConversationStore = new SqlConversationStore()

  val entries: EntryStore = new SqlEntryStore()

  val search: EntrySearch = new SqlEntrySearch()

  val ledger: UsageLedger = new SqlUsageLedger()

  /** Which profile each turn's model calls were made under. */
  val profiles: ModelProfileStore = new SqlModelProfileStore()

  /** Facts about model pairs approved while grit runs, over the seed catalog. */
  val facts: ModelFactStore = new SqlModelFactStore()

  /** Each conversation's periods, and their closing entries. */
  val periods: PeriodStore = new SqlPeriodStore(entries)

  /** The lifecycle's settings in force. */
  val lifecycle: LifecycleStore = new SqlLifecycleStore()

  /** Short read transactions, for code outside a step. */
  val db: Db = new SqlDb(dataSource)

  /** Short write transactions, for a step that records what it did as it goes. */
  val jot: Jot = new SqlJot(dataSource)

  // An edge's side: it reaches the engine only through Postgres (ADR 0002).
  private val client = new DBOSClient(dataSource)

  val inbox: Inbox = new SqlInbox(dataSource, client, conversations, entries, periods)

  /** Each plugin's cursor. */
  val cursors: PluginCursors = new SqlPluginCursors

  /** Each plugin's documents: given a plugin's name, its own, and no other plugin's. */
  val docs: PluginName -> PluginDocs = plugin => new SqlPluginDocs(plugin)

  /** Registers `turn` as the body of every turn, `close` of every attempt to close a period
    * and `post` of every posting run, and starts running what is queued; the sweep posts to
    * `plugins`, the ones enabled. Once.
    */
  def launch(
      turn: WorkflowId => Durable^ ?=> String,
      close: WorkflowId => Durable^ ?=> String,
      post: WorkflowId => Durable^ ?=> String,
      plugins: Vector[Plugin]
  ): Unit = {
    val steps = new JdbcStepFactory(dbos, dataSource)
    Turns.register(dbos, steps, turn)
    Closes.register(dbos, steps, close)
    Posts.register(dbos, steps, post)
    enabled.set(plugins.map(p => (p.name, p.version)))
    dbos.launch()
  }

  /** The enabled plugins' names and versions, set once by [[launch]]. */
  private val enabled = new AtomicReference(Vector.empty[(PluginName, Int)])

  private val sweeper =
    new Sweeper(dataSource, client, periods, lifecycle, cursors, () => enabled.get())

  /** One sweep of the lifecycle at `now`, under the settings in force: every open period
    * whose deadline has come has its attempt on that deadline enqueued
    * ([[grit.core.id.CloseRef.workflowId]]); every enabled plugin behind the newest closed
    * period has a run enqueued from its cursor ([[grit.core.plugin.PostRef]]) unless one is
    * going; then every period closed longer ago than the retention window has its turn
    * workflows and close attempts deleted, and after them its raw entries, keeping its
    * closing entry and its row ([[grit.core.store.PeriodStore.purge]]). No workflow is ever
    * deleted to be run again: what did not finish its work is reported `stuck`
    * ([[Swept]]). Only after [[launch]]. `Left` when the database fails, having done what
    * came before.
    */
  def sweep(now: Instant): Either[StoreError, Swept] = sweeper.once(now)

  private val sweeping = new AtomicBoolean(false)

  /** Sweeps at `clock`'s time every `every`, on a daemon thread of its own, until the engine
    * closes; a sweep that fails is logged, and the next one tries again, and each workflow a
    * sweep finds stuck is logged once. Once, after [[launch]].
    */
  def sweepEvery(every: FiniteDuration, clock: Clock^): Unit =
    if (sweeping.compareAndSet(false, true)) {
      val log = LoggerFactory.getLogger("grit.sweeper")
      // Read and written only by the sweeping thread.
      val logged = scala.collection.mutable.Set.empty[WorkflowId]
      val thread = new Thread(() => {
        while (sweeping.get()) {
          try
            sweep(clock.now()) match {
              case Left(e) => log.warn(s"sweep failed: $e")
              case Right(swept) =>
                swept.stuck.filterNot(logged.contains).foreach { id =>
                  logged += id
                  log.warn(s"stuck, and not run again: ${WorkflowId.value(id)}")
                }
            }
          catch { case NonFatal(e) => log.warn(s"sweep failed: $e") }
          try Thread.sleep(every.toMillis)
          catch { case _: InterruptedException => () }
        }
      })
      thread.setName("grit-sweeper")
      thread.setDaemon(true)
      thread.start()
    }

  /** The conversation `origin` names, created if it is new, as an edge's first ingest
    * would.
    */
  def conversation(origin: Origin): Either[StoreError, ConversationId] =
    transaction(conversations.findOrCreate(origin).map(_.id))

  /** Where `turn`'s workflow is, without waiting for it. */
  def status(turn: TurnRef): TurnStatus =
    try {
      val id = WorkflowId.value(turn.workflowId)
      val handle = client.retrieveWorkflow[String, Exception](id)
      Option(handle.getStatus()).map(_.status()) match {
        case None => TurnStatus.Unknown
        case Some(state) if state.isActive() => TurnStatus.Running(steps(turn))
        case Some(WorkflowState.SUCCESS) => TurnStatus.Finished(handle.getResult())
        case Some(state) => TurnStatus.Finished(s"workflow ${state.name.toLowerCase}")
      }
    } catch { case NonFatal(e) => TurnStatus.Finished(s"unreadable: ${e.getMessage}") }

  /** The steps `turn`'s workflow has recorded so far, in the order it ran them; none when
    * they cannot be read, which only dims what a watcher is shown. A step is recorded when
    * it completes, so a running step is not among them.
    */
  def steps(turn: TurnRef): Vector[RecordedStep] =
    try
      client
        .listWorkflowSteps(WorkflowId.value(turn.workflowId))
        .asScala
        .toVector
        .sortBy(_.functionId())
        .flatMap { step =>
          Option(step.functionName()).map(
            RecordedStep(_, Option(step.startedAt()), Option(step.completedAt()))
          )
        }
    catch { case NonFatal(_) => Vector.empty }

  /** The pieces `turn`'s steps wrote to its stream `key`, in order, as they are written:
    * each `next` waits for one, woken by DBOS's notification with polling behind it, and
    * the pieces end when the turn's workflow does. An edge's read (ADR 0006); what the
    * pieces mean is the writer's.
    */
  def stream(turn: TurnRef, key: String): Iterator[String] =
    client
      .readStream(WorkflowId.value(turn.workflowId), key)
      .asScala
      .collect { case piece: String => piece }

  /** Runs `body` in a transaction of its own: committed on `Right`, rolled back otherwise. */
  private def transaction[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        val result =
          try body(using Tx.fromConnection(conn))
          catch { case NonFatal(e) => conn.rollback(); throw e }
        if (result.isRight) conn.commit() else conn.rollback()
        result
      }
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }

  /** Waits for `turn` to finish and returns its workflow's output. A turn that threw
    * rethrows here.
    */
  def awaitTurn(turn: TurnRef): String =
    client.retrieveWorkflow[String, Exception](WorkflowId.value(turn.workflowId)).getResult()

  def close(): Unit =
    try {
      sweeping.set(false)
      client.close()
    } finally dbos.shutdown()
}

object Engine {

  /** Applies `schema.sql` to the database `config` names, and connects to it. The engine
    * recovers and dequeues only workflows of compatibility epoch `epoch` (ADR 0004).
    */
  def open(config: DbConfig, epoch: String): Engine^ = {
    schemaSetup(config)
    val dbos = new DBOS(
      DBOSConfig
        .defaults("grit")
        .withDatabaseUrl(config.jdbcUrl)
        .withDbUser(config.user)
        .withDbPassword(config.password)
        // Last in DBOS's precedence, so it beats both DBOS__APPVERSION and the constant
        // that enabling patching sets (DBOSExecutor's constructor).
        .withEnablePatching()
        .withAppVersion(epoch)
    )
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    new Engine(dbos, ds)
  }

  /** Applies `grit/dbos/resources/schema.sql` idempotently. */
  private def schemaSetup(config: DbConfig): Unit = {
    val sql = Option(getClass.getResourceAsStream("/schema.sql")) match {
      case Some(is) => Using.resource(is)(Source.fromInputStream(_).mkString)
      case None =>
        sys.error(
          "schema.sql not found on classpath — is grit/dbos/resources/ on the resource path?"
        )
    }
    Using.resource(
      DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    ) { conn =>
      Using.resource(conn.createStatement())(_.execute(sql))
    }
  }
}

/** Where a turn's workflow is, as an edge sees it. */
enum TurnStatus {

  /** Queued or running, having recorded these `steps` so far. */
  case Running(steps: Vector[RecordedStep])

  /** Done: the workflow's output, or why it has none. */
  case Finished(output: String)

  /** Not (yet) known to DBOS: ingested but not enqueued. */
  case Unknown
}

/** A step a turn's workflow recorded: its `name`, and when it started and completed, as
  * far as DBOS kept them.
  */
final case class RecordedStep(name: String, started: Option[Instant], completed: Option[Instant])
