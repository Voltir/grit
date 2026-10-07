package grit.dbos.engine

import java.time.Instant

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.control.NonFatal

import grit.core.document.DocumentSearch
import grit.core.durable.StepRecord
import grit.core.id.WorkflowId
import grit.core.recipe.RoomReads
import grit.core.review.ReviewStore
import grit.core.stitch.StitchStore
import grit.core.store.{
  ConversationStore,
  Db,
  EntrySearch,
  EntryStore,
  LifecycleStore,
  ModelProfileStore,
  PeriodStore,
  Principals,
  PromptStore,
  StoreError,
  UsageLedger
}
import grit.core.tool.ToolSets
import grit.core.triage.{TriageShadows, TriageStore}
import grit.core.visibility.Visibility
import grit.dbos.sql.{
  DbConfig,
  Opener,
  SqlConversationStore,
  SqlDb,
  SqlDocumentSearch,
  SqlEntrySearch,
  SqlEntryStore,
  SqlLifecycleStore,
  SqlModelProfileStore,
  SqlPeriodStore,
  SqlPrincipals,
  SqlPromptStore,
  SqlReviews,
  SqlRoomReads,
  SqlStitchStore,
  SqlToolSets,
  SqlTriageShadows,
  SqlTriageStore,
  SqlUsageLedger
}
import grit.dbos.workflow.Turns

import dev.dbos.transact.DBOSClient
import dev.dbos.transact.workflow.ListWorkflowsInput
import org.postgresql.ds.PGSimpleDataSource

/** The database `config` names, read without an engine: no DBOS executor, no lock, so nothing
  * there is recovered or run. Every session is opened `READ ONLY` in Postgres
  * (`default_transaction_read_only`), so a write through any of these stores fails with
  * `DatabaseError`. For tools reading a copy, or a live database beside its engine. Closing it
  * closes its DBOS client.
  */
trait Reader extends caps.SharedCapability, AutoCloseable {
  val db: Db
  val entries: EntryStore
  val conversations: ConversationStore
  val periods: PeriodStore
  val lifecycle: LifecycleStore
  val stitches: StitchStore
  val rooms: RoomReads
  val search: EntrySearch

  /** The enabled plugins' documents as windows draw on them. */
  val documents: DocumentSearch
  val principals: Principals
  val triage: TriageStore
  val shadows: TriageShadows
  val reviews: ReviewStore
  val ledger: UsageLedger
  val prompts: PromptStore
  val toolSets: ToolSets

  /** The profile each turn was pinned to. */
  val profiles: ModelProfileStore

  /** Every turn workflow DBOS recorded created before `until`, oldest first, each with its
    * record. `DatabaseError` when DBOS's tables cannot be read.
    */
  def turns(until: Instant): Either[StoreError, Vector[(WorkflowId, Reader.Recorded)]]

  /** `id`'s recorded steps, in the order run, each with when DBOS journaled its start; none
    * when DBOS does not know it. `DatabaseError` when DBOS's tables cannot be read.
    */
  def steps(id: WorkflowId): Either[StoreError, Vector[StepRecord]]

  /** `id`'s status, the epoch it ran under, and when it was created; `None` when DBOS does not
    * know it, or its tables cannot be read.
    */
  def workflow(id: WorkflowId): Option[Reader.Recorded]

  /** Every engine start recorded, oldest first ([[Build]]). */
  def starts(): Either[StoreError, Vector[Build.Started]]
}

object Reader {

  /** A workflow as DBOS recorded it: its `status` (`SUCCESS`, `ERROR`, `PENDING`, …), the
    * `epoch` it ran under (its application version; empty when no executor has taken it), and
    * when it was `created`.
    */
  final case class Recorded(status: String, epoch: String, created: Instant)

  /** Reads `config`'s database, resolving whom each of its transactions reads for under
    * `visibility`. Throws when the database cannot be reached.
    */
  def open(config: DbConfig, visibility: Visibility): Reader^ = {
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    // A startup parameter: Postgres itself makes every transaction on these sessions read-only,
    // whatever the driver or DBOS's client does with them.
    ds.setOptions("-c default_transaction_read_only=on")
    ds.getConnection().close()
    new Opened(ds, new DBOSClient(ds), new Opener(visibility))
  }

  private final class Opened(ds: PGSimpleDataSource, client: DBOSClient, opener: Opener)
      extends Reader {
    val db: Db = new SqlDb(ds, opener)
    val entries: EntryStore = new SqlEntryStore()
    val conversations: ConversationStore = new SqlConversationStore()
    val periods: PeriodStore = new SqlPeriodStore(entries)
    val lifecycle: LifecycleStore = new SqlLifecycleStore()
    val stitches: StitchStore = new SqlStitchStore
    val rooms: RoomReads = new SqlRoomReads
    val search: EntrySearch = new SqlEntrySearch()
    // Its keepers would write tombstones; a reader only searches, so none is ever built.
    val documents: DocumentSearch = new SqlDocumentSearch
    val principals: Principals = new SqlPrincipals()
    val triage: TriageStore = new SqlTriageStore
    val shadows: TriageShadows = new SqlTriageShadows
    val reviews: ReviewStore = new SqlReviews
    val ledger: UsageLedger = new SqlUsageLedger
    val prompts: PromptStore = new SqlPromptStore()
    val toolSets: ToolSets = new SqlToolSets()
    val profiles: ModelProfileStore = new SqlModelProfileStore

    def turns(until: Instant): Either[StoreError, Vector[(WorkflowId, Recorded)]] =
      try {
        Right(
          client
            .listWorkflows(
              new ListWorkflowsInput()
                .withWorkflowName(Turns.WorkflowName)
                .withEndTime(until)
                .withSortDesc(false)
            )
            .asScala
            .toVector
            .flatMap(s =>
              Option(s.createdAt())
                .filter(_.isBefore(until))
                .map(at =>
                  WorkflowId(s.workflowId()) ->
                    Recorded(s.status().name, Option(s.appVersion()).getOrElse(""), at)
                )
            )
            .sortBy((id, r) => (r.created, WorkflowId.value(id)))
        )
      } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }

    def steps(id: WorkflowId): Either[StoreError, Vector[StepRecord]] =
      try {
        Right(
          client
            .listWorkflowSteps(WorkflowId.value(id))
            .asScala
            .toVector
            .sortBy(_.functionId())
            .flatMap(step =>
              Option(step.functionName()).map(
                StepRecord(
                  _,
                  Option(step.output()).collect { case s: String => s },
                  Option(step.startedAt())
                )
              )
            )
        )
      } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }

    def workflow(id: WorkflowId): Option[Recorded] =
      try {
        client.getWorkflowStatus(WorkflowId.value(id)).toScala.flatMap { s =>
          Option(s.createdAt()).map(
            Recorded(s.status().name, Option(s.appVersion()).getOrElse(""), _)
          )
        }
      } catch { case NonFatal(_) => None }

    def starts(): Either[StoreError, Vector[Build.Started]] = db.read(EngineStarts.all())

    def close(): Unit = client.close()
  }
}
