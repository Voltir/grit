package grit.core.store

import grit.core.id.ConversationId
import grit.dbos.sql.TestTx

/** The conversation contract, kept by the in-memory fake. */
object InMemoryConversationTests extends ConversationContract {

  private val kept = new InMemoryEntryStore

  protected val conversations: ConversationStore = new InMemoryConversationStore(Some(kept))

  protected val entries: EntryStore = kept

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected val unknown: ConversationId = ConversationId("unknown")
}
