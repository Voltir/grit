package grit.dbos.engine

import javax.sql.DataSource

import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.clock.Clock
import grit.core.document.DocumentSearch
import grit.core.durable.StepRecord
import grit.core.edge.{Console, Desk, DeskError, Desks, Holder, RecordedStep, TurnStatus}
import grit.core.files.PieceSearch
import grit.core.host.ProcessIdentity
import grit.core.id.{ConversationId, PrincipalId, TurnRef, WorkflowId}
import grit.core.identity.Account
import grit.core.inbox.Inbox
import grit.core.place.Place
import grit.core.spend.{Budget, Spending}
import grit.core.store.{
  ConversationStore,
  Db,
  EntryStore,
  Jot,
  LifecycleStore,
  ModelProfileStore,
  Origin,
  Principals,
  PromptStore,
  StoreError,
  Tx,
  UsageLedger,
  VoiceStore
}
import grit.core.visibility.Visibility
import grit.dbos.sql.{
  DbConfig,
  Opener,
  Pool,
  SqlConversationStore,
  SqlDb,
  SqlDocumentSearch,
  SqlEntryStore,
  SqlJot,
  SqlLifecycleStore,
  SqlModelProfileStore,
  SqlPeriodStore,
  SqlPrincipals,
  SqlPromptStore,
  SqlUsageLedger,
  SqlVoiceStore
}

import dev.dbos.transact.DBOSClient
import dev.dbos.transact.exceptions.DBOSNonExistentWorkflowException
import dev.dbos.transact.workflow.{StepInfo, WorkflowState}
import com.zaxxer.hikari.HikariDataSource

/** An edge's view of a database's engine (ADR 0015): its inbox, reads, streams and turn
  * status, and the edge it registers, whether this process runs the engine ([[Engine]]) or
  * attached to one another process runs ([[Link.attach]]). Everything crosses Postgres
  * (ADR 0002), so it works while no engine runs; turns then wait.
  */
trait Link extends Console, Desks {

  /** The replies an edge has yet to post outside grit, and the parts it has. */
  val deliveries: grit.core.edge.Deliveries

  /** The messages an edge marks as being answered while their turns run. */
  val acknowledgements: grit.core.edge.Acknowledgements

  /** The review prompts an edge posts, and the labels their rater gives. */
  val reviews: grit.core.review.Reviews[grit.core.review.Prompt, grit.core.review.Verdict]

  /** Where each conversation happens, as an edge links back to one. */
  val origins: grit.core.edge.Origins

  /** Each plugin's documents put up for review, as an edge posts them. */
  val documentReviewsOf: grit.core.id.PluginName -> grit.core.review.Reviews[
    grit.core.review.DocumentReview,
    grit.core.review.ReviewWord
  ]

  /** The changes to who may see what that people make by command, and what they read of them. */
  val administration: grit.core.admin.Administration

  /** `plugins`' commands, as an edge runs them for the people it serves (ADR 0053). */
  def commands(plugins: Vector[grit.core.plugin.Plugin]): grit.core.edge.PluginCommands^

  /** The rooms an edge's bot is a member of, as the edge reports its joins and leaves. */
  val joins: grit.core.edge.Joins

  /** The working sessions an edge records for the people it serves. */
  val sessions: grit.core.edge.EdgeSessions

  /** What an edge waits on in the store for the people it serves; closed with the link. */
  val watch: grit.core.edge.EdgeWatch

  /** Waits for `turn` to finish and returns its workflow's output. A turn that threw
    * rethrows here.
    */
  def awaitTurn(turn: TurnRef): String
}

object Link {

  /** A link to the engine another process runs on the database `config` names, as grit of
    * application version `version` (as [[Engine.start]] takes it) in the process `identity`
    * names: no DBOS executor here,
    * only its client, the stores and the inbox, which takes new messages as `budget` allows,
    * dating each, and reading the day its cap counts, by `clock`, and labels the conversations
    * it creates as `visibility` labels their rooms.
    * Throws when the database cannot be reached, or when no engine has migrated DBOS's
    * schema there yet (none, or DBOS 1.0's): the next engine to start migrates it.
    */
  private[grit] def attach(
      config: DbConfig,
      version: String,
      identity: ProcessIdentity,
      budget: Budget,
      visibility: Visibility,
      clock: Clock^
  ): Link^ = {
    val pool = Pool.open(config, readOnly = false)
    try
      new Attached(config, pool, new DBOSClient(pool), version, identity, budget, visibility, clock)
    catch {
      case e: Throwable =>
        pool.close()
        throw e
    }
  }

  /** `turn`'s status through `client` ([[Link.status]]). */
  private[engine] def status(client: DBOSClient, turn: TurnRef): TurnStatus =
    try {
      val id = WorkflowId.value(turn.workflowId)
      val handle = client.retrieveWorkflow[String, Exception](id)
      Option(handle.getStatus()).map(_.status()) match {
        case None => TurnStatus.Unknown
        case Some(state) if state.isActive() => TurnStatus.Running(steps(client, turn))
        case Some(WorkflowState.SUCCESS) => TurnStatus.Finished(handle.getResult())
        case Some(state) => TurnStatus.Finished(s"workflow ${state.name.toLowerCase}")
      }
    } catch { case NonFatal(e) => TurnStatus.Finished(s"unreadable: ${e.getMessage}") }

  /** The steps the workflow `id` recorded, in order, through `client`, as a reader of its
    * history sees them ([[RecordedStep]]); a step DBOS keeps no name for is left out. Throws
    * what `client` throws.
    */
  private[dbos] def recorded(client: DBOSClient, id: WorkflowId): Vector[RecordedStep] =
    recorded(client.listWorkflowSteps(WorkflowId.value(id)))

  /** `steps`, as DBOS lists a workflow's, in order, as a reader of its history sees them
    * ([[RecordedStep]]); a step DBOS keeps no name for is left out.
    */
  private[dbos] def recorded(steps: java.util.List[StepInfo]): Vector[RecordedStep] =
    steps.asScala.toVector
      .sortBy(_.functionId())
      .flatMap { step =>
        Option(step.functionName()).map { name =>
          val output = Option(step.output()).collect { case s: String => s }
          RecordedStep(
            StepRecord(name, output, Option(step.startedAt())),
            Option(step.completedAt())
          )
        }
      }

  /** `turn`'s recorded steps through `client` ([[Link.steps]]). */
  private[engine] def steps(client: DBOSClient, turn: TurnRef): Vector[RecordedStep] =
    try recorded(client, turn.workflowId)
    catch { case NonFatal(_) => Vector.empty }

  /** `turn`'s stream `key` through `client` ([[Link.stream]]). */
  private[engine] def stream(client: DBOSClient, turn: TurnRef, key: String): Iterator[String] = {
    val pieces = client.readStream(WorkflowId.value(turn.workflowId), key).asScala
    // DBOS 1.1 throws here for a workflow it does not have, where 1.0 ended the stream.
    def more: Boolean =
      try pieces.hasNext
      catch { case _: DBOSNonExistentWorkflowException => false }
    Iterator
      .unfold(())(_ => Option.when(more)((pieces.next(), ())))
      .collect { case piece: String => piece }
  }

  /** `turn`'s output through `client` ([[Link.awaitTurn]]). */
  private[engine] def awaitTurn(client: DBOSClient, turn: TurnRef): String =
    client.retrieveWorkflow[String, Exception](WorkflowId.value(turn.workflowId)).getResult()

  /** Runs `body` in a transaction of its own on `dataSource`, at `opener`'s maintenance:
    * committed on `Right`, rolled back otherwise; the opener's error, and no body run, when the
    * transaction cannot open.
    */
  private[engine] def transaction[A](dataSource: DataSource, opener: Opener)(
      body: (Tx^) ?=> Either[StoreError, A]
  ): Either[StoreError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        val result =
          try opener.maintained(conn).flatMap(tx => body(using tx))
          catch { case NonFatal(e) => conn.rollback(); throw e }
        if (result.isRight) conn.commit() else conn.rollback()
        result
      }
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
}

/** A [[Link]] to an engine another process runs. */
private[engine] final class Attached(
    config: DbConfig,
    dataSource: HikariDataSource,
    client: DBOSClient,
    epoch: String,
    identity: ProcessIdentity,
    val budget: Budget,
    visibility: Visibility,
    clock: Clock^
) extends Link {

  private val conversations: ConversationStore = new SqlConversationStore()

  val entries: EntryStore = new SqlEntryStore()

  private val sqlLedger = new SqlUsageLedger()

  val ledger: UsageLedger = sqlLedger

  val spending: Spending = sqlLedger

  val profiles: ModelProfileStore = new SqlModelProfileStore()

  val prompts: PromptStore = new SqlPromptStore()

  val lifecycle: LifecycleStore = new SqlLifecycleStore()

  val voices: VoiceStore = new SqlVoiceStore()

  val principals: Principals = new SqlPrincipals()

  val documents: DocumentSearch = new SqlDocumentSearch

  val pieces: PieceSearch = new grit.dbos.sql.SqlPieces(new grit.dbos.sql.SqlTombstones)

  val deliveries: grit.core.edge.Deliveries = new grit.dbos.sql.SqlDeliveries()

  val acknowledgements: grit.core.edge.Acknowledgements = new grit.dbos.sql.SqlAcknowledgements()

  val reviews: grit.core.review.Reviews[grit.core.review.Prompt, grit.core.review.Verdict] =
    new grit.dbos.sql.SqlReviews

  val origins: grit.core.edge.Origins = new grit.dbos.sql.SqlOrigins

  private val documentReviews = new grit.dbos.sql.SqlDocumentReviews

  val documentReviewsOf: grit.core.id.PluginName -> grit.core.review.Reviews[
    grit.core.review.DocumentReview,
    grit.core.review.ReviewWord
  ] = documentReviews.of

  private val opener = new Opener(visibility)

  val db: Db = new SqlDb(dataSource, opener)

  val jot: Jot = new SqlJot(dataSource, opener)

  val administration: grit.core.admin.Administration =
    new grit.dbos.sql.SqlAdministration(jot, opener)

  def commands(plugins: Vector[grit.core.plugin.Plugin]): grit.core.edge.PluginCommands^ =
    new grit.dbos.sql.SqlPluginCommands(dataSource, opener, plugins)

  val joins: grit.core.edge.Joins = new grit.dbos.sql.SqlJoins(jot)

  val sessions: grit.core.edge.EdgeSessions = new grit.dbos.sql.SqlEdgeSessions(jot)

  private[dbos] val watching: SqlWatch = new SqlWatch(config, dataSource, opener)

  val watch: grit.core.edge.EdgeWatch = watching

  val inbox: Inbox =
    new SqlInbox(
      dataSource,
      client,
      conversations,
      entries,
      new SqlPeriodStore(entries),
      new grit.dbos.sql.SqlSpeechStore,
      spending,
      budget,
      new grit.dbos.sql.SqlSchedules(new grit.dbos.sql.SqlTombstones),
      visibility,
      clock
    )

  private val desks = new java.util.concurrent.ConcurrentLinkedQueue[AutoCloseable]()

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

  def holder(): Option[Holder] = EngineLock.holder(dataSource)

  def register(principal: PrincipalId, places: Set[Place]): Either[DeskError, Desk^] =
    holder() match {
      case Some(h) if h.epoch != epoch =>
        Left(
          DeskError(
            s"the engine (pid ${h.pid} on ${h.machine}) runs epoch ${h.epoch}, and this grit $epoch: " +
              "it cannot serve that engine's turns"
          )
        )
      case _ =>
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
            // The desk's only capability is its own connection, which close() closes:
            // nothing it holds outlives the link that keeps it here.
            val _ = desks.add(caps.unsafe.unsafeAssumePure(desk))
            Right(desk)
        }
    }

  def close(): Unit =
    try {
      watching.close()
      desks.forEach(_.close())
    } finally
      try client.close()
      finally dataSource.close()
}
