package grit.core.stitch

import grit.core.id.ConversationId
import grit.core.store.{
  EntryStore,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  Origin,
  PeriodStore,
  Tx
}
import grit.dbos.sql.TestTx

/** The stitch contract, kept by the in-memory fake. */
object InMemoryStitchTests extends StitchContract {

  private val store = new InMemoryEntryStore

  @caps.unsafe.untrackedCaptures
  private var origins = Map.empty[ConversationId, Origin]

  private def originOf(c: ConversationId): Origin =
    origins.getOrElse(c, Origin.Task("contract", ConversationId.value(c)))

  protected val entries: EntryStore = store
  protected val periods: PeriodStore = new InMemoryPeriodStore(store, originOf)
  protected val stitches: StitchStore = new InMemoryStitchStore(store, originOf)

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def conversation(origin: Origin): ConversationId = {
    val id = ConversationId(origin.place.written)
    origins = origins.updated(id, origin)
    id
  }
}
