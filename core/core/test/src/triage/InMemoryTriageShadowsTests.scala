package grit.core.triage

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

/** The shadows contract, kept by the in-memory fake. */
object InMemoryTriageShadowsTests extends TriageShadowsContract {

  private val store = new InMemoryEntryStore

  protected val entries: EntryStore = store
  protected val periods: PeriodStore =
    new InMemoryPeriodStore(
      store,
      c => Origin.Task("contract", ConversationId.value(c)),
      new grit.core.edge.InMemoryEdges,
      new grit.core.edge.InMemoryDeliveries
    )
  protected val triage: TriageStore = new InMemoryTriageStore(store, periods)
  protected val shadows: TriageShadows = new InMemoryTriageShadows(store, triage)

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def conversation(name: String): ConversationId = ConversationId(name)
}
