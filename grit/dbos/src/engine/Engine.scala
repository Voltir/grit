package grit.dbos.engine

import java.sql.DriverManager
import java.time.Instant

import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.durable.Durable
import grit.core.id.{ConversationId, TurnRef, WorkflowId}
import grit.core.inbox.Inbox
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
  SqlUsageLedger
}
import grit.dbos.workflow.Turns

import dev.dbos.transact.config.DBOSConfig
import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.workflow.WorkflowState
import dev.dbos.transact.{DBOS, DBOSClient}
import org.postgresql.ds.PGSimpleDataSource

/** grit over one Postgres: the stores, the turn workflow, and an edge's [[Inbox]], all in
  * this process. Open it, [[launch]] it with the turn's body, and close it when done; its
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

  /** Registers `turn` as the body of every turn and starts running queued turns. Once. */
  def launch(turn: WorkflowId => Durable^ ?=> String): Unit = {
    Turns.register(dbos, new JdbcStepFactory(dbos, dataSource), turn)
    dbos.launch()
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
    try client.close()
    finally dbos.shutdown()
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
