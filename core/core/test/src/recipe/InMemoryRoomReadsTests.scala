package grit.core.recipe

import grit.core.id.{ConversationId, EntryId, PrincipalId}
import grit.core.store.{
  EntryStore,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  InMemoryPrincipals,
  Origin,
  PeriodStore,
  Tx
}
import grit.dbos.sql.TestTx

/** The room reads contract, kept by the in-memory fake. */
object InMemoryRoomReadsTests extends RoomReadsContract {

  private val store = new InMemoryEntryStore
  private val principals = new InMemoryPrincipals

  @caps.unsafe.untrackedCaptures
  private var origins = Map.empty[ConversationId, Origin]

  private def originOf(c: ConversationId): Origin =
    origins.getOrElse(c, Origin.Task("contract", ConversationId.value(c)))

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new InMemoryPeriodStore(store, originOf)
  protected val rooms: RoomReads = new InMemoryRoomReads(store, originOf, principals)

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def conversation(origin: Origin): ConversationId = {
    val id = ConversationId(origin.place.written)
    origins = origins.updated(id, origin)
    id
  }

  protected def authored(entry: EntryId, by: PrincipalId): Unit = principals.authored(entry, by)
}
