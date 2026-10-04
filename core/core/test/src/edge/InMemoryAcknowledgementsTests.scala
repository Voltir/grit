package grit.core.edge

import grit.core.id.ConversationId
import grit.core.store.Tx
import grit.dbos.sql.TestTx

/** The acknowledgements contract, kept by the in-memory fake. */
object InMemoryAcknowledgementsTests extends AcknowledgementsContract {

  protected def fresh(): (Acknowledgements, ConversationId) =
    (new InMemoryAcknowledgements, ConversationId("c1"))

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
}
