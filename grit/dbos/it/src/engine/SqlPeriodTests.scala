package grit.dbos.engine

import grit.core.id.ConversationId
import grit.core.store.{EntryStore, LifecycleStore, Origin, PeriodContract, PeriodStore, Tx}
import grit.dbos.sql.{LiveDb, SqlEntryStore, SqlLifecycleStore, SqlPeriodStore, TestPostgres}

/** The period contract, kept by the SQL stores against a real Postgres. */
object SqlPeriodTests extends PeriodContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_period")
    Engine.open(c, "test").close()
    c
  }

  private val store = new SqlEntryStore()

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new SqlPeriodStore(store)
  protected val lifecycle: LifecycleStore = new SqlLifecycleStore()

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

  protected def conversation(name: String): ConversationId =
    LiveDb.conversation(config, origin(name)).id

  protected def origin(name: String): Origin = Origin.Task("contract", name)
}
