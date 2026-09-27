package grit.core.store

import grit.core.id.ConversationId
import grit.dbos.sql.TestTx

/** The conversation contract, kept by the in-memory fake. */
object InMemoryConversationTests extends ConversationContract {

  protected val conversations: ConversationStore = new InMemoryConversationStore

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected val unknown: ConversationId = ConversationId("unknown")
}
