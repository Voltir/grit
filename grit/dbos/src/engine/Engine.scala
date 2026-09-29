package grit.dbos.engine

import java.sql.DriverManager
import java.time.Instant
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.clock.Clock
import grit.core.durable.Durable
import grit.core.edge.{Desk, DeskError, EdgeDirectory, ToolRequests}
import grit.core.host.ProcessIdentity
import grit.core.id.{ConversationId, PluginName, PrincipalId, TurnRef, WorkflowId}
import grit.core.inbox.Inbox
import grit.core.place.Place
import grit.core.plugin.{CacheDocs, Plugin, PluginCursors, PluginDocs}
import grit.core.spend.{Budget, Spending}
import grit.core.store.{
  ClosedPeriod,
  ConversationStore,
  Db,
  EntrySearch,
  EntryStore,
  Jot,
  LifecycleStore,
  ModelProfileStore,
  ModelSettingStore,
  Origin,
  PeriodStore,
  Principals,
  PromptStore,
  StoreError,
  Tombstones,
  UsageLedger,
  VoiceStore
}
import grit.core.tool.ToolSets
import grit.core.triage.TriageStore
import grit.dbos.sql.{
  DbConfig,
  SqlCacheDocs,
  SqlConversationStore,
  SqlDb,
  SqlEdgeDirectory,
  SqlEntrySearch,
  SqlEntryStore,
  SqlJot,
  SqlLifecycleStore,
  SqlModelProfileStore,
  SqlModelSettingStore,
  SqlPeriodStore,
  SqlPluginCursors,
  SqlPluginDocs,
  SqlPrincipals,
  SqlPromptStore,
  SqlTombstones,
  SqlToolRequests,
  SqlToolSets,
  SqlTriageStore,
  SqlUsageLedger,
  SqlVoiceStore
}
import grit.dbos.workflow.{Closes, Posts, Running, Settles, Triages, Turns}

import dev.dbos.transact.config.DBOSConfig
import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.workflow.{ListWorkflowsInput, WorkflowState}
import dev.dbos.transact.{DBOS, DBOSClient}
import org.postgresql.ds.PGSimpleDataSource
import org.slf4j.LoggerFactory

/** grit over one Postgres: the stores, the turn, close, settle, posting and triage workflows, the
  * sweep that closes periods, and an edge's [[Inbox]], all in this process, under the
  * database's [[EngineLock]]. Open it, [[launch]] it with the workflows' bodies, start its
  * [[sweepEvery]], and close it when done; its threads keep the JVM alive until then.
  */
final class Engine private (
    dbos: DBOS,
    dataSource: PGSimpleDataSource,
    lock: EngineLock^,
    config: DbConfig,
    identity: ProcessIdentity,
    val budget: Budget
) extends Link {

  val conversations: ConversationStore = new SqlConversationStore()

  val entries: EntryStore = new SqlEntryStore()

  val search: EntrySearch = new SqlEntrySearch()

  private val sqlLedger = new SqlUsageLedger()

  val ledger: UsageLedger = sqlLedger

  val spending: Spending = sqlLedger

  /** Which profile each turn's model calls were made under. */
  val profiles: ModelProfileStore = new SqlModelProfileStore()

  /** The system prompt each turn was sent, its fragments kept by id (ADR 0016). */
  val prompts: PromptStore = new SqlPromptStore()

  /** Every tool set a turn was offered, by id. */
  val toolSets: ToolSets = new SqlToolSets()

  /** The requests hosted tool calls become (ADR 0017). */
  val requests: ToolRequests = new SqlToolRequests()

  /** Which live edge serves each place, and what it offers there. */
  val edgeDirectory: EdgeDirectory = new SqlEdgeDirectory()

  /** Settings of model pairs approved while grit runs, over the seed catalog. */
  val modelSettings: ModelSettingStore = new SqlModelSettingStore()

  /** Each conversation's periods, and their closing entries. */
  val periods: PeriodStore = new SqlPeriodStore(entries)

  /** The lifecycle's settings in force. */
  val lifecycle: LifecycleStore = new SqlLifecycleStore()

  /** The voice grit talks to the person in. */
  val voices: VoiceStore = new SqlVoiceStore()

  val principals: Principals = new SqlPrincipals()

  val deliveries: grit.core.edge.Deliveries = new grit.dbos.sql.SqlDeliveries()

  /** What grit has decided to delete, and when. */
  val tombstones: Tombstones = new SqlTombstones

  /** What triage made of each heard message. */
  val triage: TriageStore = new SqlTriageStore

  /** Short read transactions, for code outside a step. */
  val db: Db = new SqlDb(dataSource)

  /** Short write transactions, for a step that records what it did as it goes. */
  val jot: Jot = new SqlJot(dataSource)

  // An edge's side: it reaches the engine only through Postgres (ADR 0002).
  private val client = new DBOSClient(dataSource)

  val inbox: Inbox =
    new SqlInbox(dataSource, client, conversations, entries, periods, spending, budget)

  /** Each plugin's cursor. */
  val cursors: PluginCursors = new SqlPluginCursors(tombstones)

  /** Each plugin's documents: given a plugin's name, its own, and no other plugin's. */
  val docs: PluginName -> PluginDocs = plugin => new SqlPluginDocs(plugin)

  /** Given a plugin and the closed period it is posting, where it keeps what it makes of it. */
  val cache: (PluginName, ClosedPeriod) -> CacheDocs =
    (plugin, closed) => new SqlCacheDocs(plugin, closed.order)

  /** Registers `turn` as the body of every turn, `close` of every attempt to close a period,
    * `settle` of every question whether anyone is waiting on a quiet period, `post` of every
    * posting run, and `triage` of every heard message's triage, and starts running what is
    * queued; the sweep posts to `plugins`, the ones enabled. Once.
    */
  def launch(
      turn: WorkflowId => Durable^ ?=> String,
      close: WorkflowId => Durable^ ?=> String,
      settle: WorkflowId => Durable^ ?=> String,
      post: WorkflowId => Durable^ ?=> String,
      triage: WorkflowId => Durable^ ?=> String,
      plugins: Vector[Plugin]
  ): Unit = {
    val steps = new JdbcStepFactory(dbos, dataSource)
    Turns.register(dbos, steps, turn, running)
    Closes.register(dbos, steps, close, running)
    Settles.register(dbos, steps, settle, running)
    Posts.register(dbos, steps, post, running)
    Triages.register(dbos, steps, triage, running)
    enabled.set(plugins.map(p => (p.name, p.version)))
    dbos.launch()
  }

  /** The enabled plugins' names and versions, set once by [[launch]]. */
  private val enabled = new AtomicReference(Vector.empty[(PluginName, Int)])

  private val sweeper =
    new Sweeper(
      dataSource,
      client,
      conversations,
      entries,
      periods,
      ledger,
      profiles,
      prompts,
      lifecycle,
      tombstones,
      cursors,
      () => enabled.get()
    )

  /** One sweep of the lifecycle at `now`, under the settings in force: every open period
    * whose deadline has come has its attempt on that deadline enqueued
    * ([[grit.core.id.CloseRef.workflowId]]), and every other that is to be asked whether anyone
    * is waiting on it ([[grit.core.period.Deadline.ask]]) has its question enqueued
    * ([[grit.core.id.SettleRef.workflowId]]); every enabled plugin behind the newest closed
    * period has a run enqueued from its cursor ([[grit.core.plugin.PostRef]]) unless one is
    * going, and every other plugin with a cursor is marked for deletion
    * ([[grit.core.retention.Target.Disabled]]); then every tombstone whose kind's retention has passed is collected
    * ([[grit.core.retention.Target]]): its workflows deleted, unless one is still queued or
    * running, which defers it to a later sweep, then its rows. No workflow is ever
    * deleted to be run again: what did not finish its work is reported `stuck`
    * ([[Swept]]). Only after [[launch]]. `Left` when the database fails, having done what
    * came before.
    */
  def sweep(now: Instant): Either[StoreError, Swept] = sweeper.once(now)

  /** How many workflows are queued or running, a turn's, close's, question's, posting's or
    * triage's; none once everything enqueued has ended. `Left` when DBOS's tables cannot be
    * read.
    */
  def unfinished(): Either[StoreError, Int] =
    try
      Right(
        client
          .listWorkflows(
            new ListWorkflowsInput()
              .withStatus(WorkflowState.PENDING, WorkflowState.ENQUEUED, WorkflowState.DELAYED)
          )
          .size()
      )
    catch {
      case NonFatal(e) =>
        Left(StoreError.DatabaseError(Option(e.getMessage).getOrElse(e.getClass.getSimpleName)))
    }

  private val sweeping = new AtomicBoolean(false)

  /** The sweeping thread, once [[sweepEvery]] started it. */
  private val sweepThread = new AtomicReference(Option.empty[Thread])

  /** The desks registered through this engine: [[close]] closes them. */
  private val desks = new java.util.concurrent.ConcurrentLinkedQueue[AutoCloseable]()

  /** Registers an edge in this process for `principal`, hosting `places`, and opens its desk
    * (ADR 0017): live until it or the engine closes. `Left` when the database cannot be
    * reached.
    */
  def register(principal: PrincipalId, places: Set[Place]): Either[DeskError, Desk^] =
    SqlDesk.open(config, dataSource, client, principal, places, identity) match {
      case Left(e) => Left(e)
      case Right(desk) =>
        // The desk's only capability is its own connection, which close() closes: nothing it
        // holds outlives the engine that keeps it here.
        val _ = desks.add(caps.unsafe.unsafeAssumePure(desk))
        Right(desk)
    }

  /** The workflow bodies running in this process: [[close]] waits for them. */
  private val running = new Running

  private val closed = new AtomicBoolean(false)

  /** Writes the lock's heartbeat every beat, on a daemon thread, until the engine closes; a
    * heartbeat the lock refuses closes the engine. Once, from [[Engine.start]].
    */
  private def beating(): Unit = {
    val log = LoggerFactory.getLogger("grit.engine")
    val thread = new Thread(() => {
      var alive = true
      while (alive && !closed.get()) {
        try Thread.sleep(lock.beat.toMillis)
        catch { case _: InterruptedException => () }
        if (!closed.get() && !lock.beatOnce()) {
          alive = false
          log.warn("lost the engine lock: stopping, so no workflow runs beside its new holder")
          close()
        }
      }
    })
    thread.setName("grit-heartbeat")
    thread.setDaemon(true)
    thread.start()
  }

  /** Sweeps at `clock`'s time every `every`, on a daemon thread of its own, until the engine
    * closes; a sweep that fails is logged, and the next one tries again, each workflow a
    * sweep finds stuck is logged once, and so is each plugin it newly marks as not enabled. Once, after [[launch]].
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
                swept.disabled.foreach { p =>
                  log.warn(
                    s"plugin ${PluginName.value(p)} is not enabled: its documents and cursor " +
                      "are deleted after the ledger window unless it is enabled again"
                  )
                }
            }
          catch { case NonFatal(e) => log.warn(s"sweep failed: $e") }
          try Thread.sleep(every.toMillis)
          catch { case _: InterruptedException => () }
        }
      })
      thread.setName("grit-sweeper")
      thread.setDaemon(true)
      sweepThread.set(Some(thread))
      thread.start()
    }

  /** The conversation `origin` names, created by `by` if it is new, as an edge's first
    * ingest would.
    */
  def conversation(origin: Origin, by: PrincipalId): Either[StoreError, ConversationId] =
    Link.transaction(dataSource)(conversations.findOrCreate(origin, by).map(_.id))

  def status(turn: TurnRef): TurnStatus = Link.status(client, turn)

  def steps(turn: TurnRef): Vector[RecordedStep] = Link.steps(client, turn)

  def stream(turn: TurnRef, key: String): Iterator[String] = Link.stream(client, turn, key)

  def awaitTurn(turn: TurnRef): String = Link.awaitTurn(client, turn)

  /** The engine holding this database's lock: this one, while it holds it. */
  def holder(): Option[Holder] = EngineLock.holder(config)

  /** Stops sweeping, stops DBOS, waits up to [[Engine.BodiesWithin]] for every workflow body
    * still running (interrupted by the stop) to return, then releases the lock: no body this
    * engine ran outlives its lock, unless one ignores its interrupt past the wait, which is
    * logged. Also what losing the lock does. Once; later calls return at once.
    */
  def close(): Unit =
    if (closed.compareAndSet(false, true)) {
      try {
        sweeping.set(false)
        sweepThread.get().foreach { t =>
          t.interrupt()
          t.join(Engine.BodiesWithin.toMillis)
        }
        desks.forEach(_.close())
        try client.close()
        finally dbos.shutdown()
        if (!running.awaitNone(Engine.BodiesWithin))
          LoggerFactory
            .getLogger("grit.engine")
            .warn(s"workflow bodies still running after ${Engine.BodiesWithin}: releasing the lock")
      } finally lock.close()
    }
}

object Engine {

  /** How long [[Engine.close]] waits for running workflow bodies: 30 seconds. */
  val BodiesWithin: FiniteDuration = 30.seconds

  /** [[EngineLock.take]], then [[start]]. */
  def open(
      config: DbConfig,
      epoch: String,
      identity: ProcessIdentity,
      budget: Budget
  ): Either[NotTaken, Engine^] =
    EngineLock.take(config) match {
      case Left(refused) => Left(refused)
      case Right(lock) => Right(start(config, lock, epoch, identity, budget))
    }

  /** The engine of the database `config` names, which `lock` is held on: its schema applied,
    * `lock`'s row written, naming this process as `identity` says, and its heartbeat begun,
    * recovering and dequeuing only workflows of
    * compatibility epoch `epoch` (ADR 0004). Losing the lock (its connection dropped, or its
    * row gone or taken) stops the engine as [[Engine.close]] does, and an edge's calls on it
    * then fail. Its inbox takes new messages as `budget` allows. Closes `lock` when it
    * throws.
    */
  def start(
      config: DbConfig,
      lock: EngineLock^,
      epoch: String,
      identity: ProcessIdentity,
      budget: Budget
  ): Engine^ =
    try {
      schemaSetup(config)
      lock
        .claim(epoch, identity)
        .left
        .foreach(why => sys.error(s"the engine's row could not be written: $why"))
      val engine = build(config, lock, epoch, identity, budget)
      engine.beating()
      engine
    } catch {
      case e: Throwable =>
        lock.close()
        throw e
    }

  private def build(
      config: DbConfig,
      lock: EngineLock^,
      epoch: String,
      identity: ProcessIdentity,
      budget: Budget
  ): Engine^ = {
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
    new Engine(dbos, ds, lock, config, identity, budget)
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
