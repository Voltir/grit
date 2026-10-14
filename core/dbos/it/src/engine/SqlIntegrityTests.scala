package grit.dbos.engine

import grit.core.id.ConversationId
import grit.core.store.{EntryStore, IntegrityContract, Origin, PeriodStore, Tx}
import grit.dbos.sql.{LiveDb, SqlEntryStore, SqlPeriodStore, TestPostgres}

/** The integrity contract, kept by the SQL stores against a real Postgres. */
object SqlIntegrityTests extends IntegrityContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_integrity")
    LiveEngine.open(c, "test").close()
    c
  }

  private val store = new SqlEntryStore()

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new SqlPeriodStore(store)

  protected def transaction[A](body: (Tx^) ?=> A): A =
    LiveDb.under(config, IntegrityContract.Capping)(body)

  protected def conversation(origin: Origin): ConversationId =
    LiveDb.conversation(config, origin).id
}
