package grit.dbos.engine

import java.time.Instant

import scala.jdk.OptionConverters.*
import scala.util.control.NonFatal

import grit.core.id.WorkflowId
import grit.core.stitch.StitchStore
import grit.core.store.{
  ConversationStore,
  Db,
  EntrySearch,
  EntryStore,
  LifecycleStore,
  Principals,
  StoreError
}
import grit.core.triage.TriageStore
import grit.dbos.sql.{
  DbConfig,
  SqlConversationStore,
  SqlDb,
  SqlEntrySearch,
  SqlEntryStore,
  SqlLifecycleStore,
  SqlPrincipals,
  SqlStitchStore,
  SqlTriageStore
}

import dev.dbos.transact.DBOSClient
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
  val lifecycle: LifecycleStore
  val stitches: StitchStore
  val search: EntrySearch
  val principals: Principals
  val triage: TriageStore

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

  /** Throws when the database cannot be reached. */
  def open(config: DbConfig): Reader^ = {
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    // A startup parameter: Postgres itself makes every transaction on these sessions read-only,
    // whatever the driver or DBOS's client does with them.
    ds.setOptions("-c default_transaction_read_only=on")
    ds.getConnection().close()
    new Opened(ds, new DBOSClient(ds))
  }

  private final class Opened(ds: PGSimpleDataSource, client: DBOSClient) extends Reader {
    val db: Db = new SqlDb(ds)
    val entries: EntryStore = new SqlEntryStore()
    val conversations: ConversationStore = new SqlConversationStore()
    val lifecycle: LifecycleStore = new SqlLifecycleStore()
    val stitches: StitchStore = new SqlStitchStore
    val search: EntrySearch = new SqlEntrySearch()
    val principals: Principals = new SqlPrincipals()
    val triage: TriageStore = new SqlTriageStore

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
