package grit.dbos.engine

import grit.core.id.{ConversationId, EntryId, PrincipalId}
import grit.core.identity.Account
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

  protected def authored(entry: EntryId, by: Account): Unit =
    LiveDb.transaction(config) {
      new SqlPrincipals()
        .name(by, Account.written(by))
        .flatMap(_ => LiveDb.authored(entry, by))
        .fold(e => sys.error(s"arranging an author: $e"), identity)
    }

  protected def person(account: Account): PrincipalId = LiveDb.principal(config, account)
}
