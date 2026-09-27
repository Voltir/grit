package grit.core.store

import grit.core.id.ConversationId
import grit.core.tool.ToolSets
import grit.dbos.sql.TestTx

/** The store contract, kept by the in-memory fakes. */
object InMemoryStoreTests extends StoreContract {

  protected val entries: EntryStore = new InMemoryEntryStore
  protected val ledger: UsageLedger = new InMemoryUsageLedger
  protected val profiles: ModelProfileStore = new InMemoryModelProfileStore
  protected val facts: ModelFactStore = new InMemoryModelFactStore
  protected val toolSets: ToolSets = new InMemoryToolSets

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def conversation(name: String): ConversationId = ConversationId(name)

  protected val unknownConversation: ConversationId = ConversationId("unknown")
}
