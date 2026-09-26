package grit.core.store

import grit.core.id.ConversationId
import grit.dbos.sql.TestTx

/** The period contract, kept by the in-memory fakes. */
object InMemoryPeriodTests extends PeriodContract {

  private val store = new InMemoryEntryStore

  protected val entries: EntryStore = store
  protected val periods: PeriodStore =
    new InMemoryPeriodStore(store, c => Origin.Task("contract", ConversationId.value(c)))
  protected val lifecycle: LifecycleStore = new InMemoryLifecycleStore

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def conversation(name: String): ConversationId = ConversationId(name)

  protected def origin(name: String): Origin = Origin.Task("contract", name)
}
