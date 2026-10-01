package grit.dbos.engine

import grit.core.id.ConversationId
import grit.core.stitch.{StitchContract, StitchStore}
import grit.core.store.{EntryStore, Origin, PeriodStore, Tx}
import grit.dbos.sql.{LiveDb, SqlEntryStore, SqlPeriodStore, SqlStitchStore, TestPostgres}

/** The stitch contract, kept by the SQL store against a real Postgres. */
object SqlStitchTests extends StitchContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_stitch")
    LiveEngine.open(c, "test").close()
    c
  }

  private val store = new SqlEntryStore()

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new SqlPeriodStore(store)
  protected val stitches: StitchStore = new SqlStitchStore

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

  protected def conversation(origin: Origin): ConversationId =
    LiveDb.conversation(config, origin).id
}
