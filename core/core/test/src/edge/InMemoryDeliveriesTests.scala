package grit.core.edge

import grit.core.id.ConversationId
import grit.core.store.Tx
import grit.dbos.sql.TestTx

/** The deliveries contract, kept by the in-memory fake. */
object InMemoryDeliveriesTests extends DeliveriesContract {

  protected def fresh(): (Deliveries, ConversationId) =
    (new InMemoryDeliveries, ConversationId("c1"))

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
}
