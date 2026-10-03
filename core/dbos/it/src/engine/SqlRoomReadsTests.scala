package grit.dbos.engine

import scala.util.Using

import grit.core.id.{ConversationId, EntryId, PrincipalId}
import grit.core.recipe.{RoomReads, RoomReadsContract}
import grit.core.store.{EntryStore, Origin, PeriodStore, Tx}
import grit.dbos.sql.{
  LiveDb,
  SqlEntryStore,
  SqlPeriodStore,
  SqlPrincipals,
  SqlRoomReads,
  TestPostgres
}

/** The room reads contract, kept by the SQL reads against a real Postgres. */
object SqlRoomReadsTests extends RoomReadsContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_room_reads")
    LiveEngine.open(c, "test").close()
    c
  }

  private val store = new SqlEntryStore()

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new SqlPeriodStore(store)
  protected val rooms: RoomReads = new SqlRoomReads

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

  protected def conversation(origin: Origin): ConversationId =
    LiveDb.conversation(config, origin).id

  protected def authored(entry: EntryId, by: PrincipalId): Unit =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      new SqlPrincipals()
        .enroll(by, PrincipalId.value(by))
        .fold(e => sys.error(s"arranging a person: $e"), identity)
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement("INSERT INTO grit.inbound (entry_id, author) VALUES (?, ?)")
      ) { ps =>
        ps.setString(1, EntryId.value(entry))
        ps.setString(2, PrincipalId.value(by))
        val _ = ps.executeUpdate()
      }
    }
}
