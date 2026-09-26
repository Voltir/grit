package grit.core.store

import grit.core.id.{ConversationId, PeriodRef}
import grit.dbos.sql.TestTx

/** The period contract, kept by the in-memory fakes. */
object InMemoryPeriodTests extends PeriodContract {

  private val store = new InMemoryEntryStore

  protected val entries: EntryStore = store
  private val fake =
    new InMemoryPeriodStore(store, c => Origin.Task("contract", ConversationId.value(c)))
  protected val periods: PeriodStore = fake
  protected val lifecycle: LifecycleStore = new InMemoryLifecycleStore

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def conversation(name: String): ConversationId = ConversationId(name)

  protected def origin(name: String): Origin = Origin.Task("contract", name)

  protected def verdictsOn(period: PeriodRef): Int = fake.verdictsOn(period)
}
