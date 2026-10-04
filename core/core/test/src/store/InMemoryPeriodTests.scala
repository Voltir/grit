package grit.core.store

import grit.core.id.{ConversationId, PeriodRef}
import grit.dbos.sql.TestTx

/** The period contract, kept by the in-memory fakes. */
object InMemoryPeriodTests extends PeriodContract {

  private val store = new InMemoryEntryStore

  protected val entries: EntryStore = store
  private val edges = new grit.core.edge.InMemoryEdges
  private val delivering = new grit.core.edge.InMemoryDeliveries
  private val acknowledging = new grit.core.edge.InMemoryAcknowledgements
  private val fake =
    new InMemoryPeriodStore(
      store,
      c => Origin.Task("contract", ConversationId.value(c)),
      edges,
      delivering,
      acknowledging
    )
  protected val requests: grit.core.edge.ToolRequests = edges
  protected val deliveries: grit.core.edge.Deliveries = delivering
  protected val acknowledgements: grit.core.edge.Acknowledgements = acknowledging
  protected val periods: PeriodStore = fake
  protected val lifecycle: LifecycleStore = new InMemoryLifecycleStore

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def conversation(name: String): ConversationId = ConversationId(name)

  protected def origin(name: String): Origin = Origin.Task("contract", name)

  protected def verdictsOn(period: PeriodRef): Int = fake.verdictsOn(period)
}
