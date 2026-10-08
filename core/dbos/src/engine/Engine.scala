package grit.dbos.engine

import java.sql.DriverManager
import java.time.Instant
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.clock.Clock
import grit.core.document.{DocumentKeeper, DocumentStore, DocumentTerms}
import grit.core.durable.Durable
import grit.core.edge.{Desk, DeskError, EdgeDirectory, ToolRequests}
import grit.core.host.ProcessIdentity
import grit.core.id.{ConversationId, JobName, PluginName, PrincipalId, TurnRef, WorkflowId}
import grit.core.identity.{Account, Domain, Identities, Principal, Realm}
import grit.core.inbox.Inbox
import grit.core.job.{ScheduleDesk, ScheduleStore}
import grit.core.place.Place
import grit.core.plugin.{CacheDocs, Plugin, PluginCursors, PluginReads}
import grit.core.speech.SpeechStore
import grit.core.spend.{Budget, Spending}
import grit.core.store.{
  Askers,
  ClosedPeriod,
  ConversationStore,
  Db,
  EntrySearch,
  EntryStore,
  Jot,
  LifecycleStore,
  Linking,
  ModelProfileStore,
  ModelSettingStore,
  Origin,
  PeriodStore,
  Principals,
  PromptStore,
  StoreError,
  Tombstones,
  Tx,
  UsageLedger,
  VoiceStore,
  Voucher
}
import grit.core.tool.ToolSets
import grit.core.triage.{Shadowing, TriageShadows, TriageStore}
import grit.core.visibility.{Compartment, Visibility}
import grit.dbos.sql.{
  DbConfig,
  Opener,
  SqlCacheDocs,
  SqlConversationStore,
  SqlDb,
  SqlDocuments,
  SqlEdgeDirectory,
  SqlEntrySearch,
  SqlEntryStore,
  SqlJot,
  SqlLabels,
  SqlLifecycleStore,
  SqlModelProfileStore,
  SqlModelSettingStore,
  SqlPeriodStore,
  SqlPluginCursors,
  SqlPluginDocs,
  SqlPrincipals,
  SqlPromptStore,
  SqlSchedules,
  SqlSpeechStore,
  SqlTombstones,
  SqlToolRequests,
  SqlToolSets,
  SqlTriageShadows,
  SqlTriageStore,
  SqlUsageLedger,
  SqlVoiceStore,
  SqlVoucher
}
import grit.dbos.workflow.{
  Closes,
  DurableWorkflow,
  Posts,
  Running,
  Runs,
  Settles,
  Shadows,
  Stitches,
  Triages,
  Turns
}

import dev.dbos.transact.config.DBOSConfig
import dev.dbos.transact.migrations.MigrationManager
import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.workflow.{ListWorkflowsInput, WorkflowState}
import dev.dbos.transact.{DBOS, DBOSClient}
import org.postgresql.ds.PGSimpleDataSource
import org.slf4j.LoggerFactory

/** grit over one Postgres: the stores, the turn, close, settle, posting, triage, placement, shadow and job run workflows, the
  * sweep that closes periods, and an edge's [[Inbox]], all in this process, under the
  * database's [[EngineLock]]. Open it, [[launch]] it with the workflows' bodies, start its
  * [[sweepEvery]], and close it when done; its threads keep the JVM alive until then.
  */
final class Engine private (
    dbos: DBOS,
    dataSource: PGSimpleDataSource,
    lock: EngineLock^,
    config: DbConfig,
    epoch: String,
    identity: ProcessIdentity,
    val budget: Budget,
    /** What it was opened under: the compartments its database runs under, and its rooms'
      * labels.
      */
    val visibility: Visibility,
    clock: Clock^
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

  /** The voucher for `realms` ([[Voucher]]), keeping an attested email only in one of
    * `domains`: what a deployment records its trusted attesters' answers with, for the realms it
    * trusts each for and the domains it claims.
    */
  def voucher(realms: Set[Realm], domains: Set[Domain]): Voucher =
    new SqlVoucher(realms, domains, visibility)

  /** Ends now, in a transaction of its own, every attestation `identities` would not make: of an
    * account no realm it trusts holds, or holding an email in no domain it claims. Each is kept
    * as no full member, with no email. Each account it moves home, or whose membership it ends,
    * is one [[Linking]]. Only a start serving a deployment's edges calls it, with its identities:
    * an engine opened for a chat, a tool or the eval calls nothing, since its identities need
    * not be all the deployment trusts. What it ends, the account's next
    * answer from a realm still trusted makes again. It writes no attestation already ended, so a
    * second start under the same identities writes nothing.
    */
  def untrust(identities: Identities): Either[StoreError, Vector[Linking]] =
    Link.transaction(dataSource, opener)(
      new SqlVoucher(identities.realms, identities.domains, visibility).untrust
    )

  val deliveries: grit.core.edge.Deliveries = new grit.dbos.sql.SqlDeliveries()

  /** The messages an edge marks as being answered while their turns run. */
  val acknowledgements: grit.core.edge.Acknowledgements = new grit.dbos.sql.SqlAcknowledgements()

  /** What grit has decided to delete, and when. */
  val tombstones: Tombstones = new SqlTombstones

  /** What triage made of each heard message. */
  val triage: TriageStore = new SqlTriageStore

  /** What each declared shadow variant made of heard messages already triaged. */
  val shadows: TriageShadows = new SqlTriageShadows

  /** Where each heard message could be answered, and grit's decisions on each. */
  val speech: SpeechStore = new SqlSpeechStore

  /** Which heard messages a review considered, their prompts, and the labels standing on
    * them.
    */
  val reviews: grit.core.review.ReviewStore = new grit.dbos.sql.SqlReviews

  /** Where each thread's first message was placed among its room's exchanges (ADR 0023). */
  val stitches: grit.core.stitch.StitchStore = new grit.dbos.sql.SqlStitchStore

  /** What each room said before a time, as a pool reads it. */
  val rooms: grit.core.recipe.RoomReads = new grit.dbos.sql.SqlRoomReads

  /** How each transaction the engine opens gets its clearance. */
  private val opener = new Opener(visibility)

  /** Short read transactions, for code outside a step. */
  val db: Db = new SqlDb(dataSource, opener)

  /** Who each turn answers, as this engine's transactions resolve it. */
  val askers: Askers = {
    val resolving = opener
    new Askers {
      def of(turn: TurnRef)(using Tx^): Either[StoreError, Option[Principal]] =
        resolving.asker(turn)
    }
  }

  /** Short write transactions, for a step that records what it did as it goes. */
  val jot: Jot = new SqlJot(dataSource, opener)

  // An edge's side: it reaches the engine only through Postgres (ADR 0002). Built with no
  // application name, so what it enqueues is unclaimed (application_name NULL), which DBOS
  // dequeues for this executor's application by `application_name = 'grit' OR IS NULL`.
  private val client = new DBOSClient(dataSource)

  private val sqlSchedules = new SqlSchedules(tombstones)

  val inbox: Inbox =
    new SqlInbox(
      dataSource,
      client,
      conversations,
      entries,
      periods,
      speech,
      spending,
      budget,
      sqlSchedules,
      visibility,
      clock
    )

  /** Where each opening is placed, by the `stitch` workflow [[launch]] registers. */
  val placements: grit.core.stitch.Placements = new DbosPlacements(client)

  /** Every job's schedules (ADR 0029), as the engine keeps them. */
  val schedules: ScheduleStore = sqlSchedules

  /** `plugin`'s desk (ADR 0029): the schedules its tools write, of the jobs named `jobs`, each
    * in a transaction of its own, dated by the clock the engine was opened with.
    */
  def desk(plugin: PluginName, jobs: Vector[JobName]): ScheduleDesk^ =
    sqlSchedules.desk(plugin, jobs, jot, clock)

  /** Each plugin's cursor. */
  val cursors: PluginCursors = new SqlPluginCursors(tombstones)

  private val sqlDocuments = new SqlDocuments(tombstones)

  /** Every plugin's documents (ADR 0028), as windows draw on them and as the engine itself
    * writes them.
    */
  val documents: DocumentStore = sqlDocuments

  /** Each plugin's own documents as its tools and its service read them: given a plugin's
    * name, its own, and no other plugin's.
    */
  val reads: PluginName -> PluginReads =
    plugin => PluginReads(new SqlPluginDocs(plugin), sqlDocuments.shelf(plugin))

  /** Given a plugin and its terms, its documents as its posting writes them. */
  val keeper: (PluginName, DocumentTerms) -> DocumentKeeper =
    (plugin, terms) => sqlDocuments.keeper(plugin, terms)

  /** Given a plugin and the closed period it is posting, where it keeps what it makes of it. */
  val cache: (PluginName, ClosedPeriod) -> CacheDocs =
    (plugin, closed) => new SqlCacheDocs(plugin, closed.order)

  /** Registers `turn` as the body of every turn, `close` of every attempt to close a period,
    * `settle` of every question whether anyone is waiting on a quiet period, `post` of every
    * posting run, `triage` of every heard message's triage, `stitch` of every opening's
    * placement ([[grit.core.id.StitchRef]]), and `shadow` of every shadow
    * ([[grit.core.id.ShadowRef]]), and `run` of every job's run (a turn of its slot's
    * conversation, [[grit.core.job.Slot]]), and starts running what is queued; `plugins` are the ones
    * enabled, and the sweep posts to those with something to post ([[Plugin.posts]]), and enqueues shadows for `shadowing`, the variants declared.
    * Without them no shadow is enqueued, and one an earlier engine left queued ends at once,
    * keeping nothing. The terms of `plugins`' documents are declared the ones in force
    * ([[DocumentStore.declare]]) before anything runs. Makes the engine's epoch the
    * database's latest application version, so work enqueued without one (every grit
    * enqueue) runs here whatever epochs the database has seen. Once. Throws when the terms
    * cannot be recorded, as DBOS's own launch throws when it cannot start.
    */
  def launch(
      turn: WorkflowId => Durable^ ?=> String,
      close: WorkflowId => Durable^ ?=> String,
      settle: WorkflowId => Durable^ ?=> String,
      post: WorkflowId => Durable^ ?=> String,
      triage: WorkflowId => Durable^ ?=> String,
      stitch: WorkflowId => Durable^ ?=> String,
      plugins: Vector[Plugin],
      shadow: WorkflowId => Durable^ ?=> String = Engine.Unshadowed,
      shadowing: Vector[Shadowing] = Vector.empty,
      run: WorkflowId => Durable^ ?=> String = Engine.Unrun
  ): Unit = {
    val steps = new JdbcStepFactory(dbos, dataSource)
    Turns.register(dbos, steps, opener, turn, running)
    Closes.register(dbos, steps, opener, close, running)
    Settles.register(dbos, steps, opener, settle, running)
    Posts.register(dbos, steps, opener, post, running)
    Triages.register(dbos, steps, opener, triage, running)
    Stitches.register(dbos, steps, opener, stitch, running)
    Shadows.register(dbos, steps, opener, shadow, running)
    Runs.register(dbos, steps, opener, run, running)
    enabled.set(plugins.map(p => (p.name, p.version)))
    posting.set(plugins.filter(_.posts).map(p => (p.name, p.version)))
    declared.set(shadowing)
    Link
      .transaction(dataSource, opener)(
        documents.declare(plugins.flatMap(p => p.documents.map(d => p.name -> d.terms)))
      )
      .left
      .foreach(e => throw new IllegalStateException(s"the plugins' document terms: $e"))
    // Before launch, so the queues are there when recovery puts work back on them.
    Turns.registerQueue(client)
    Posts.registerQueue(client)
    Shadows.registerQueue(client)
    Stitches.registerQueue(client)
    dbos.launch()
    // DBOS 1.1 dequeues a workflow enqueued with no version only on the latest version
    // (QueuesDAO.versionClause); the lock holder is the database's one engine, so it is that.
    dbos.setLatestApplicationVersion(epoch)
  }

  /** The enabled plugins' names and versions, set once by [[launch]]. */
  private val enabled = new AtomicReference(Vector.empty[(PluginName, Int)])

  /** The enabled plugins with something to post, and their versions, set once by [[launch]]. */
  private val posting = new AtomicReference(Vector.empty[(PluginName, Int)])

  /** The shadow variants declared, set once by [[launch]]. */
  private val declared = new AtomicReference(Vector.empty[Shadowing])

  private val sweeper =
    new Sweeper(
      dataSource,
      client,
      conversations,
      entries,
      periods,
      ledger,
      speech,
      profiles,
      prompts,
      lifecycle,
      tombstones,
      cursors,
      shadows,
      documents,
      sqlSchedules,
      () => enabled.get(),
      () => posting.get(),
      () => declared.get(),
      opener
    )

  /** One sweep of the lifecycle at `now`, under the settings in force: every open period
    * whose deadline has come has its attempt on that deadline enqueued
    * ([[grit.core.id.CloseRef.workflowId]]), and every other that is to be asked whether anyone
    * is waiting on it ([[grit.core.period.Deadline.ask]]), but a job's run's
    * ([[grit.core.job.Slot.of]]), which closes on its deadline, has its question enqueued
    * ([[grit.core.id.SettleRef.workflowId]]); every enabled plugin with something to post behind the newest closed
    * period has a run enqueued from its cursor ([[grit.core.plugin.PostRef]]) unless one is
    * going, and every other plugin with a cursor, documents or document terms is marked for deletion
    * ([[grit.core.retention.Target.Disabled]]); every declared shadow variant none of whose
    * shadows is queued or running has its oldest unshadowed messages enqueued, as many as its
    * day's cap covers ([[grit.core.triage.Shadowing.batch]]); then every tombstone whose kind's retention (for a document version, its plugin's declared one) has passed is collected
    * ([[grit.core.retention.Target]]): its workflows deleted, unless one is still queued or
    * running, which defers it to a later sweep, then its rows. No workflow is ever
    * deleted to be run again: what did not finish its work is reported `stuck`
    * ([[Swept]]). Only after [[launch]]. `Left` when the database fails, having done what
    * came before.
    */
  def sweep(now: Instant): Either[StoreError, Swept] = sweeper.once(now)

  /** How many workflows are queued or running, a turn's, close's, question's, posting's,
    * triage's, placement's or shadow's; none once everything enqueued has ended. `Left` when DBOS's tables cannot be
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

  /** The threads [[every]] started: [[close]] stops them. */
  private val passes = new java.util.concurrent.ConcurrentLinkedQueue[Thread]()

  /** The desks registered through this engine: [[close]] closes them. */
  private val desks = new java.util.concurrent.ConcurrentLinkedQueue[AutoCloseable]()

  /** Registers an edge in this process for `principal`, hosting `places`, and opens its desk
    * (ADR 0017): live until it or the engine closes. `Left` when the database cannot be
    * reached.
    */
  def register(principal: PrincipalId, places: Set[Place]): Either[DeskError, Desk^] =
    SqlDesk.open(
      config,
      dataSource,
      client,
      principal,
      places,
      identity,
      opener
    ) match {
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

  /** Runs `pass` every `period` on a daemon thread of its own, named `name`, until the engine
    * closes: one pass at a time, the next `period` after the last returned, so no two passes
    * ever overlap. A pass that throws is logged under `name`, and the next one runs. Nothing
    * runs once the engine has closed: [[close]] interrupts a pass under way and waits up to
    * [[Engine.BodiesWithin]] for it to return. Only an engine has this: it holds the database's lock (ADR 0015), and an
    * attached [[Link]] does not.
    */
  def every(name: String, period: FiniteDuration)(pass: () => Unit): Unit =
    if (!closed.get()) {
      val log = LoggerFactory.getLogger(name)
      val thread = new Thread(() => {
        while (!closed.get()) {
          try pass()
          catch {
            // The engine closing interrupts a pass under way; the loop then ends.
            case _: InterruptedException => ()
            case NonFatal(e) => log.warn(s"$name failed: $e")
          }
          try Thread.sleep(period.toMillis)
          catch { case _: InterruptedException => () }
        }
      })
      thread.setName(name)
      thread.setDaemon(true)
      passes.add(thread)
      thread.start()
    }

  /** Sweeps every `period` ([[every]], as `grit.sweeper`), at the time by the clock the engine
    * was opened with; a sweep that fails
    * is logged, and the next one tries again, each workflow a sweep finds stuck is logged once,
    * and so is each plugin it newly marks as not enabled. Once, after [[launch]]; later calls
    * do nothing.
    */
  def sweepEvery(period: FiniteDuration): Unit =
    if (sweeping.compareAndSet(false, true)) {
      val log = LoggerFactory.getLogger("grit.sweeper")
      // Read and written only by the sweeping thread.
      val logged = scala.collection.mutable.Set.empty[WorkflowId]
      every("grit.sweeper", period) { () =>
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
      }
    }

  def conversation(origin: Origin, by: Account): Either[StoreError, ConversationId] =
    Link.transaction(dataSource, opener)(
      grit.dbos.sql.SqlRooms
        .label(visibility, origin)
        .flatMap(conversations.findOrCreate(origin, by, _))
        .map(_.id)
    )

  def status(turn: TurnRef): TurnStatus = Link.status(client, turn)

  def steps(turn: TurnRef): Vector[RecordedStep] = Link.steps(client, turn)

  def stream(turn: TurnRef, key: String): Iterator[String] = Link.stream(client, turn, key)

  def awaitTurn(turn: TurnRef): String = Link.awaitTurn(client, turn)

  /** The engine holding this database's lock: this one, while it holds it. */
  def holder(): Option[Holder] = EngineLock.holder(config)

  /** Stops every pass ([[every]]) and the sweep, stops DBOS, waits up to [[Engine.BodiesWithin]] for every workflow body
    * still running (interrupted by the stop) to return, then releases the lock: no body this
    * engine ran outlives its lock, unless one ignores its interrupt past the wait, which is
    * logged. Also what losing the lock does. Once; later calls return at once.
    */
  def close(): Unit =
    if (closed.compareAndSet(false, true)) {
      try {
        passes.forEach(_.interrupt())
        passes.forEach(_.join(Engine.BodiesWithin.toMillis))
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

  /** The shadow body of an engine that declares no shadows: it asks nothing and keeps
    * nothing.
    */
  val Unshadowed: WorkflowId -> Durable^ ?-> String =
    id => (_: Durable^) ?=> s"no shadows are declared: ${WorkflowId.value(id)}"

  /** The run body of an engine given none: it runs no job and writes nothing, so its run ends
    * without a reply ([[grit.core.job.InFlight.Ended]]).
    */
  val Unrun: WorkflowId -> Durable^ ?-> String =
    id => (_: Durable^) ?=> s"no job runs here: ${WorkflowId.value(id)}"

  /** How long [[Engine.close]] waits for running workflow bodies: 30 seconds. */
  val BodiesWithin: FiniteDuration = 30.seconds

  /** [[EngineLock.take]], then [[start]]. */
  def open(
      config: DbConfig,
      epoch: String,
      identity: ProcessIdentity,
      budget: Budget,
      visibility: Visibility,
      clock: Clock^
  ): Either[Unopened, Engine^] =
    EngineLock.take(config) match {
      case Left(refused) => Left(Unopened.Lock(refused))
      case Right(lock) => start(config, lock, epoch, identity, budget, visibility, clock)
    }

  /** The engine of the database `config` names, which `lock` is held on: its schema and
    * DBOS's applied (DBOS's migrations run here, under `lock`), `lock`'s row written, naming this process as `identity` says,
    * and this start, with [[Build.current]], appended to `grit.engine_starts` in the same
    * statement, and its heartbeat begun,
    * recovering and dequeuing only workflows of
    * compatibility epoch `epoch` (ADR 0004). Losing the lock (its connection dropped, or its
    * row gone or taken) stops the engine as [[Engine.close]] does, and an edge's calls on it
    * then fail. Its inbox takes new messages as `budget` allows, dating each, and reading the
    * day its cap counts, by `clock`. `visibility`'s compartments
    * are recorded as the ones the database runs under before anything else is written
    * ([[Unopened.Dropped]] when they drop one it ran under, having closed `lock` and claimed
    * nothing). Throws, having closed `lock`, when either schema cannot be applied.
    */
  def start(
      config: DbConfig,
      lock: EngineLock^,
      epoch: String,
      identity: ProcessIdentity,
      budget: Budget,
      visibility: Visibility,
      clock: Clock^
  ): Either[Unopened, Engine^] =
    try {
      schemaSetup(config)
      val dbosConfig = dbosConfigOf(config, epoch)
      // Before anything builds a DBOSClient, which refuses a database DBOS has not migrated.
      MigrationManager.runMigrations(dbosConfig)
      compartmentsSetup(config, visibility) match {
        case Some(dropped) =>
          lock.close()
          Left(Unopened.Dropped(dropped))
        case None =>
          lock
            .claim(epoch, identity, Build.current)
            .left
            .foreach(why => sys.error(s"the engine's row could not be written: $why"))
          val engine = build(config, dbosConfig, lock, epoch, identity, budget, visibility, clock)
          engine.beating()
          Right(engine)
      }
    } catch {
      case e: Throwable =>
        lock.close()
        throw e
    }

  /** Records `visibility`'s compartments as the ones the database runs under
    * ([[SqlLabels.reconcile]]); the one they drop, if any. Throws when they cannot be read or
    * written, as a schema that cannot be applied does.
    */
  private def compartmentsSetup(config: DbConfig, visibility: Visibility): Option[Compartment] =
    Using.resource(
      DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    ) { conn =>
      conn.setAutoCommit(false)
      SqlLabels.reconcile(visibility.compartments)(using
        new Opener(visibility).maintained(conn)
      ) match {
        case Right(dropped) =>
          conn.commit()
          dropped
        case Left(e) =>
          conn.rollback()
          sys.error(s"the compartments could not be recorded: $e")
      }
    }

  /** DBOS's configuration for the database `config` names, recovering and dequeuing only
    * workflows of epoch `epoch`.
    */
  private def dbosConfigOf(config: DbConfig, epoch: String): DBOSConfig =
    DBOSConfig
      .defaults(DurableWorkflow.ApplicationName)
      .withDatabaseUrl(config.jdbcUrl)
      .withDbUser(config.user)
      .withDbPassword(config.password)
      // Last in DBOS's precedence, so it beats both DBOS__APPVERSION and the constant
      // that enabling patching sets (DBOSExecutor's constructor).
      .withEnablePatching()
      .withAppVersion(epoch)

  private def build(
      config: DbConfig,
      dbosConfig: DBOSConfig,
      lock: EngineLock^,
      epoch: String,
      identity: ProcessIdentity,
      budget: Budget,
      visibility: Visibility,
      clock: Clock^
  ): Engine^ = {
    val dbos = new DBOS(dbosConfig)
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    new Engine(dbos, ds, lock, config, epoch, identity, budget, visibility, clock)
  }

  /** Applies `core/dbos/resources/schema.sql` idempotently. */
  private def schemaSetup(config: DbConfig): Unit = {
    val sql = Option(getClass.getResourceAsStream("/schema.sql")) match {
      case Some(is) => Using.resource(is)(Source.fromInputStream(_).mkString)
      case None =>
        sys.error(
          "schema.sql not found on classpath — is core/dbos/resources/ on the resource path?"
        )
    }
    Using.resource(
      DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    ) { conn =>
      Using.resource(conn.createStatement())(_.execute(sql))
    }
  }
}

/** Why an engine did not open. */
enum Unopened {

  /** Its database's lock was not taken. */
  case Lock(why: NotTaken)

  /** `compartment`, among those its database last ran under, is not declared now: a label
    * holding it could then be read by a clearance that could not read it before.
    */
  case Dropped(compartment: Compartment)

  /** One line for a person, at `now`. */
  def message(now: Instant): String = this match {
    case Lock(why) => why.message(now)
    case Dropped(c) =>
      s"this database ran under the compartment ${Compartment.name(c)}, which the deployment no longer declares: dropping or renaming a compartment is refused"
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
