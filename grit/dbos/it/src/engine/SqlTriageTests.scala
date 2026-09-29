package grit.dbos.engine

import grit.core.id.ConversationId
import grit.core.store.{EntryStore, Origin, PeriodStore, Tx}
import grit.core.triage.{TriageContract, TriageStore}
import grit.dbos.sql.{LiveDb, SqlEntryStore, SqlPeriodStore, SqlTriageStore, TestPostgres}

/** The triage contract, kept by the SQL store against a real Postgres. */
object SqlTriageTests extends TriageContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_triage")
    LiveEngine.open(c, "test").close()
    c
  }

  private val store = new SqlEntryStore()

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new SqlPeriodStore(store)
  protected val triage: TriageStore = new SqlTriageStore

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

  protected def conversation(name: String): ConversationId =
    LiveDb.conversation(config, Origin.Task("contract", name)).id
}
