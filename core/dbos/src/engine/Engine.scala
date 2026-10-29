package grit.dbos.engine

import java.sql.DriverManager
import java.time.Instant
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.clock.Clock
import grit.core.durable.{Durable, StepRecord}
import grit.core.edge.{Desk, DeskError}
import grit.core.host.ProcessIdentity
import grit.core.id.{ConversationId, PluginName, PrincipalId, TurnRef, WorkflowId}
import grit.core.identity.{Account, Domain, Identities, Realm}
import grit.core.inbox.{Inbox, InboxError}
import grit.core.place.Place
import grit.core.plugin.Plugin
import grit.core.spend.Budget
import grit.core.store.{Db, Jot, Linking, Origin, StoreError, Voucher}
import grit.core.triage.Shadowing
import grit.core.visibility.{Compartment, Visibility}
import grit.dbos.sql.{DbConfig, Opener, Pool, SqlDb, SqlJot, SqlLabels, SqlVoucher}
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
import com.zaxxer.hikari.HikariDataSource
import org.slf4j.LoggerFactory

/** grit over one Postgres: the stores, the turn, close, settle, posting, triage, placement, shadow and job run workflows, the
  * sweep that closes periods, and an edge's [[Inbox]], all in this process, under the
  * database's [[EngineLock]]. Open it, [[launch]] it with the workflows' bodies, start its
  * [[sweepEvery]], and close it when done; its threads keep the JVM alive until then.
  */
final class Engine private (
    dbos: DBOS,
    dataSource: HikariDataSource,
    lock: EngineLock^,
    config: DbConfig,
    epoch: String,
    identity: ProcessIdentity,
    val budget: Budget,
    /** What it was opened under: the compartments its database runs under, and its rooms'
      * labels.
      */
    val visibility: Visibility,
    /** What it was opened with to tell the time by: the clock its inbox, sweep and desks date
      * by.
      */
    val clock: Clock^
) extends Stores(visibility),
      Link,
      Runtime {

  /** The voucher for `realms` ([[Voucher]]), keeping an attested email only in one of
    * `domains`: what a deployment records its trusted attesters' answers with, for the realms it
    * trusts each for and the domains it claims.
    */
  def voucher(realms: Set[Realm], domains: Set[Domain]): Voucher =
    new SqlVoucher(realms, domains)

  /** Ends now, in a transaction of its own, every attestation `identities` would not make: of an
    * account no realm it trusts holds, or holding an email in no domain it claims. Each is kept
    * as no full member, with no email. Each account it moves home, or whose membership it ends,
    * is one [[Linking]]. Only a start built from a whole deployment, one serving its edges or
    * catching them up, calls it, with its identities: an engine opened for a chat, a tool or the
    * eval calls nothing, since a chat may be declared with fewer edges, and so trust fewer
    * realms. What it ends, the account's next
    * answer from a realm still trusted makes again. It writes no attestation already ended, so a
    * second start under the same identities writes nothing.
    */
  def untrust(identities: Identities): Either[StoreError, Vector[Linking]] =
    Link.transaction(dataSource, opener)(
      new SqlVoucher(identities.realms, identities.domains).untrust
    )

  /** Short read transactions, for code outside a step. */
  val db: Db = new SqlDb(dataSource, opener)

  /** Short write transactions, for a step that records what it did as it goes. */
  val jot: Jot = new SqlJot(dataSource, opener)

  val administration: grit.core.admin.Administration =
    new grit.dbos.sql.SqlAdministration(jot, opener)

  def commands(plugins: Vector[grit.core.plugin.Plugin]): grit.core.edge.PluginCommands^ =
    new grit.dbos.sql.SqlPluginCommands(dataSource, opener, plugins)

  val joins: grit.core.edge.Joins = new grit.dbos.sql.SqlJoins(jot)

  val sessions: grit.core.edge.EdgeSessions = new grit.dbos.sql.SqlEdgeSessions(jot)

  private[dbos] val watching: SqlWatch = new SqlWatch(config, dataSource, opener)

  val watch: grit.core.edge.EdgeWatch = watching

  // An edge's side: it reaches the engine only through Postgres (ADR 0002). Built with no
  // application name, so what it enqueues is unclaimed (application_name NULL), which DBOS
  // dequeues for this executor's application by `application_name = 'grit' OR IS NULL`.
  private val client = new DBOSClient(dataSource)

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

  /** [[Inbox.startTurn]], through this engine's [[inbox]]. */
  def startTurn(turn: TurnRef): Either[InboxError, Unit] = inbox.startTurn(turn)

  /** Registers `turn` as the body of every turn, `close` of every attempt to close a period,
    * `settle` of every question whether anyone is waiting on a quiet period, `post` of every
    * posting run, `triage` of every heard message's triage, `stitch` of every opening's
    * placement ([[grit.core.id.StitchRef]]), and `shadow` of every shadow
    * ([[grit.core.id.ShadowRef]]), and `run` of every job's run (a turn of its slot's
    * conversation, [[grit.core.job.Slot]]), and starts running what is queued; `plugins` are the ones
    * enabled, and the sweep posts to those with something to post ([[Plugin.posts]]), and enqueues shadows for `shadowing`, the variants declared.
    * Without them no shadow is enqueued, and one an earlier engine left queued ends at once,
    * keeping nothing. The terms of `plugins`' documents are declared the ones in force
    * ([[grit.core.document.DocumentStore.declare]]), and each one's documents swept ([[grit.core.plugin.Documents.sweep]]), at
    * every label, before anything runs. Makes the engine's epoch the
    * database's latest application version, so work enqueued without one (every grit
    * enqueue) runs here whatever epochs the database has seen. Once. Throws when the terms
    * cannot be recorded or a sweep fails, as DBOS's own launch throws when it cannot start.
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
  ): Unit = {
    val steps = new JdbcStepFactory(dbos, dataSource)
    DurableWorkflow.register(dbos, steps, opener, Turns.WorkflowName, turn, running)
    DurableWorkflow.register(dbos, steps, opener, Closes.WorkflowName, close, running)
    DurableWorkflow.register(dbos, steps, opener, Settles.WorkflowName, settle, running)
    DurableWorkflow.register(dbos, steps, opener, Posts.WorkflowName, post, running)
    DurableWorkflow.register(dbos, steps, opener, Triages.WorkflowName, triage, running)
    DurableWorkflow.register(dbos, steps, opener, Stitches.WorkflowName, stitch, running)
    DurableWorkflow.register(dbos, steps, opener, Shadows.WorkflowName, shadow, running)
    DurableWorkflow.register(dbos, steps, opener, Runs.WorkflowName, run, running)
    enabled.set(plugins.map(p => (p.name, p.version)))
    posting.set(plugins.filter(_.posts).map(p => (p.name, p.version)))
    declared.set(shadowing)
    val declaring = plugins.flatMap(p => p.documents.map(d => (p.name, d)))
    Link
      .transaction(dataSource, opener)(
        documents
          .declare(declaring.map((name, d) => name -> d.terms))
          .flatMap(_ =>
            declaring.foldLeft[Either[StoreError, Unit]](Right(())) { case (done, (name, d)) =>
              done.flatMap(_ => d.sweep(keeper(name, d.terms), clock.now()).map(_ => ()))
            }
          )
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
      documentReviews,
      sqlSchedules,
      files,
      pieces,
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
    * day's cap covers ([[grit.core.triage.Shadowing.batch]]); every working session whose log or
    * row has been kept long enough since it ended is marked for deletion
    * ([[grit.core.retention.Target.SessionEvents]], [[grit.core.retention.Target.Session]]); then every tombstone whose kind's retention (for a document version, its plugin's declared one) has passed is collected
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
        .label(origin)
        .flatMap(conversations.findOrCreate(origin, by, _))
        .map(_.id)
    )

  def status(turn: TurnRef): TurnStatus = Link.status(client, turn)

  def steps(turn: TurnRef): Vector[RecordedStep] = Link.steps(client, turn)

  def stream(turn: TurnRef, key: String): Iterator[String] = Link.stream(client, turn, key)

  def awaitTurn(turn: TurnRef): String = Link.awaitTurn(client, turn)

  /** The engine holding this database's lock: this one, while it holds it. */
  def holder(): Option[Holder] = EngineLock.holder(dataSource)

  /** Stops every pass ([[every]]) and the sweep, lapses every watch ([[watch]]), stops DBOS, waits up to [[Engine.BodiesWithin]] for every workflow body
    * still running (interrupted by the stop) to return, closes its pool, then releases the lock: no body this
    * engine ran outlives its lock, unless one ignores its interrupt past the wait, which is
    * logged. Also what losing the lock does. Once; later calls return at once.
    */
  def close(): Unit =
    if (closed.compareAndSet(false, true)) {
      try {
        passes.forEach(_.interrupt())
        passes.forEach(_.join(Engine.BodiesWithin.toMillis))
        watching.close()
        desks.forEach(_.close())
        try client.close()
        finally dbos.shutdown()
        if (!running.awaitNone(Engine.BodiesWithin))
          LoggerFactory
            .getLogger("grit.engine")
            .warn(s"workflow bodies still running after ${Engine.BodiesWithin}: releasing the lock")
      } finally
        try dataSource.close()
        finally lock.close()
    }
}

object Engine {

  /** How long [[Engine.close]] waits for running workflow bodies: 30 seconds. */
  val BodiesWithin: FiniteDuration = 30.seconds

  /** [[EngineLock.take]], then [[start]]. */
  private[grit] def open(
      config: DbConfig,
      version: String,
      identity: ProcessIdentity,
      budget: Budget,
      visibility: Visibility,
      clock: Clock^
  ): Either[Unopened, Engine^] =
    EngineLock.take(config) match {
      case Left(refused) => Left(Unopened.Lock(refused))
      case Right(lock) => start(config, lock, version, identity, budget, visibility, clock)
    }

  /** The engine of the database `config` names, which `lock` is held on: its schema and
    * DBOS's applied (DBOS's migrations run here, under `lock`), `lock`'s row written, naming this process as `identity` says,
    * and this start, with [[Build.current]], appended to `grit.engine_starts` in the same
    * statement, and its heartbeat begun,
    * recovering and dequeuing only workflows of
    * application version `version` (ADR 0004: a compatibility epoch, and the deployment's own
    * version after a `+`). Losing the lock (its connection dropped, or its
    * row gone or taken) stops the engine as [[Engine.close]] does, and an edge's calls on it
    * then fail. Its inbox takes new messages as `budget` allows, dating each, and reading the
    * day its cap counts, by `clock`. `visibility`'s compartments
    * are recorded as the ones the database runs under before anything else is written
    * ([[Unopened.Dropped]] when they drop one it ran under, having closed `lock` and claimed
    * nothing). Throws, having closed `lock`, when either schema cannot be applied, or what the
    * database records of rooms and groups beside the declaration cannot be read.
    */
  private[grit] def start(
      config: DbConfig,
      lock: EngineLock^,
      version: String,
      identity: ProcessIdentity,
      budget: Budget,
      visibility: Visibility,
      clock: Clock^
  ): Either[Unopened, Engine^] =
    try {
      schemaSetup(config)
      val dbosConfig = dbosConfigOf(config, version)
      // Before anything builds a DBOSClient, which refuses a database DBOS has not migrated.
      MigrationManager.runMigrations(dbosConfig)
      compartmentsSetup(config, visibility) match {
        case Some(dropped) =>
          lock.close()
          Left(Unopened.Dropped(dropped))
        case None =>
          lock
            .claim(version, identity, Build.current)
            .left
            .foreach(why => sys.error(s"the engine's row could not be written: $why"))
          val engine = build(config, dbosConfig, lock, version, identity, budget, visibility, clock)
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
    * written, or the transaction recording them cannot open, as a schema that cannot be applied
    * does.
    */
  private def compartmentsSetup(config: DbConfig, visibility: Visibility): Option[Compartment] =
    Using.resource(
      DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    ) { conn =>
      conn.setAutoCommit(false)
      new Opener(visibility)
        .maintained(conn)
        .flatMap(tx => SqlLabels.reconcile(visibility.compartments)(using tx)) match {
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
    val pool = Pool.open(config, readOnly = false)
    try new Engine(dbos, pool, lock, config, epoch, identity, budget, visibility, clock)
    catch {
      case e: Throwable =>
        pool.close()
        throw e
    }
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

/** A step a turn's workflow recorded: what a reader of its history sees of it (`record`), and
  * when it completed, as far as DBOS kept it.
  */
final case class RecordedStep(record: StepRecord, completed: Option[Instant]) {
  def name: String = record.name
  def started: Option[Instant] = record.started
}
