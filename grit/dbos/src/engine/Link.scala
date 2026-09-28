package grit.dbos.engine

import javax.sql.DataSource

import scala.jdk.CollectionConverters.*
import scala.util.Using
import scala.util.control.NonFatal

import grit.core.edge.{Desk, DeskError}
import grit.core.host.ProcessIdentity
import grit.core.id.{ConversationId, PrincipalId, TurnRef, WorkflowId}
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
import grit.dbos.sql.{
  DbConfig,
  SqlConversationStore,
  SqlDb,
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
import dev.dbos.transact.workflow.WorkflowState
import org.postgresql.ds.PGSimpleDataSource

/** An edge's view of a database's engine (ADR 0015): its inbox, reads, streams and turn
  * status, and the edge it registers, whether this process runs the engine ([[Engine]]) or
  * attached to one another process runs ([[Link.attach]]). Everything crosses Postgres
  * (ADR 0002), so it works while no engine runs; turns then wait.
  */
trait Link extends caps.SharedCapability, AutoCloseable {

  val entries: EntryStore

  val ledger: UsageLedger

  /** What the ledger's calls cost, read back. */
  val spending: Spending

  /** How this link's inbox meters new messages: the cap, and where days begin. */
  val budget: Budget

  /** Which profile each turn's model calls were made under. */
  val profiles: ModelProfileStore

  /** The system prompt each turn was sent. */
  val prompts: PromptStore

  /** The lifecycle's settings in force. */
  val lifecycle: LifecycleStore

  /** The voice grit talks to the person in. */
  val voices: VoiceStore

  /** The people an edge enrolled, and whose names a window shows. */
  val principals: Principals

  /** The replies an edge has yet to post outside grit, and the parts it has. */
  val deliveries: grit.core.edge.Deliveries

  /** Short read transactions. */
  val db: Db

  /** Short write transactions. */
  val jot: Jot

  val inbox: Inbox

  /** The conversation `origin` names, created by `by` if it is new, as an edge's first
    * ingest would.
    */
  def conversation(origin: Origin, by: PrincipalId): Either[StoreError, ConversationId]

  /** Where `turn`'s workflow is, without waiting for it. */
  def status(turn: TurnRef): TurnStatus

  /** The steps `turn`'s workflow has recorded so far, in the order it ran them; none when
    * they cannot be read, which only dims what a watcher is shown. A step is recorded when
    * it completes, so a running step is not among them.
    */
  def steps(turn: TurnRef): Vector[RecordedStep]

  /** The pieces `turn`'s steps wrote to its stream `key`, in order, as they are written:
    * each `next` waits for one, woken by DBOS's notification with polling behind it, and
    * the pieces end when the turn's workflow does. An edge's read (ADR 0006); what the
    * pieces mean is the writer's.
    */
  def stream(turn: TurnRef, key: String): Iterator[String]

  /** Waits for `turn` to finish and returns its workflow's output. A turn that threw
    * rethrows here.
    */
  def awaitTurn(turn: TurnRef): String

  /** The engine holding the database's lock; `None` when none holds it: turns wait until one
    * does.
    */
  def holder(): Option[Holder]

  /** Registers an edge in this process for `principal`, hosting `places`, and opens its desk
    * (ADR 0017): live until it or this link closes. `Left` when the database cannot be
    * reached, or the engine running it is of another compatibility epoch than this link's.
    */
  def register(principal: PrincipalId, places: Set[Place]): Either[DeskError, Desk^]
}

object Link {

  /** A link to the engine another process runs on the database `config` names, as grit of
    * compatibility epoch `epoch` in the process `identity` names: no DBOS executor here,
    * only its client, the stores and the inbox, which takes new messages as `budget` allows.
    * Throws when the database cannot be reached.
    */
  def attach(
      config: DbConfig,
      epoch: String,
      identity: ProcessIdentity,
      budget: Budget
  ): Link^ = {
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    new Attached(config, ds, new DBOSClient(ds), epoch, identity, budget)
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

  /** `turn`'s recorded steps through `client` ([[Link.steps]]). */
  private[engine] def steps(client: DBOSClient, turn: TurnRef): Vector[RecordedStep] =
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

  /** `turn`'s stream `key` through `client` ([[Link.stream]]). */
  private[engine] def stream(client: DBOSClient, turn: TurnRef, key: String): Iterator[String] =
    client
      .readStream(WorkflowId.value(turn.workflowId), key)
      .asScala
      .collect { case piece: String => piece }

  /** `turn`'s output through `client` ([[Link.awaitTurn]]). */
  private[engine] def awaitTurn(client: DBOSClient, turn: TurnRef): String =
    client.retrieveWorkflow[String, Exception](WorkflowId.value(turn.workflowId)).getResult()

  /** Runs `body` in a transaction of its own on `dataSource`: committed on `Right`, rolled
    * back otherwise.
    */
  private[engine] def transaction[A](dataSource: DataSource)(
      body: (Tx^) ?=> Either[StoreError, A]
  ): Either[StoreError, A] =
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
}

/** A [[Link]] to an engine another process runs. */
private[engine] final class Attached(
    config: DbConfig,
    dataSource: PGSimpleDataSource,
    client: DBOSClient,
    epoch: String,
    identity: ProcessIdentity,
    val budget: Budget
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

  val deliveries: grit.core.edge.Deliveries = new grit.dbos.sql.SqlDeliveries()

  val db: Db = new SqlDb(dataSource)

  val jot: Jot = new SqlJot(dataSource)

  val inbox: Inbox =
    new SqlInbox(
      dataSource,
      client,
      conversations,
      entries,
      new SqlPeriodStore(entries),
      spending,
      budget
    )

  private val desks = new java.util.concurrent.ConcurrentLinkedQueue[AutoCloseable]()

  def conversation(origin: Origin, by: PrincipalId): Either[StoreError, ConversationId] =
    Link.transaction(dataSource)(conversations.findOrCreate(origin, by).map(_.id))

  def status(turn: TurnRef): TurnStatus = Link.status(client, turn)

  def steps(turn: TurnRef): Vector[RecordedStep] = Link.steps(client, turn)

  def stream(turn: TurnRef, key: String): Iterator[String] = Link.stream(client, turn, key)

  def awaitTurn(turn: TurnRef): String = Link.awaitTurn(client, turn)

  def holder(): Option[Holder] = EngineLock.holder(config)

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
        SqlDesk.open(config, dataSource, client, principal, places, identity) match {
          case Left(e) => Left(e)
          case Right(desk) =>
            // The desk's only capability is its own connection, which close() closes:
            // nothing it holds outlives the link that keeps it here.
            val _ = desks.add(caps.unsafe.unsafeAssumePure(desk))
            Right(desk)
        }
    }

  def close(): Unit =
    try desks.forEach(_.close())
    finally client.close()
}
