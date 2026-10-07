package grit.dbos.engine

import grit.core.id.ConversationId
import grit.core.recipe.RoomReads
import grit.core.stitch.StitchStore
import grit.core.store.{ClearanceContract, EntryStore, Origin, PeriodStore, Tx}
import grit.core.visibility.{Clearance, Label}
import grit.dbos.sql.{
  LiveDb,
  SqlEntryStore,
  SqlPeriodStore,
  SqlRoomReads,
  SqlStitchStore,
  TestPostgres
}

/** The clearance contract, kept by the SQL stores against a real Postgres. */
object SqlClearanceTests extends ClearanceContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_clearance")
    LiveEngine.open(c, "test").close()
    c
  }

  private val store = new SqlEntryStore()

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new SqlPeriodStore(store)
  protected val stitches: StitchStore = new SqlStitchStore
  protected val rooms: RoomReads = new SqlRoomReads

  protected def transaction[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    LiveDb.transaction(config, clearance)(body)

  protected def conversation(origin: Origin, label: Label): ConversationId =
    LiveDb.conversation(config, origin, label).id
}
