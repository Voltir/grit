package grit.core.edge

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.identity.Account
import grit.core.message.Message
import grit.core.store.{
  Conversation,
  Entry,
  InMemoryEntryStore,
  InMemoryPrincipals,
  Origin,
  Payload,
  Tx
}
import grit.core.visibility.{Clearance, Integrity}
import grit.dbos.sql.TestTx

/** The deliveries contract, kept by the in-memory fake. */
object InMemoryDeliveriesTests extends DeliveriesContract {

  protected def fresh(): (Deliveries, ConversationId) =
    (new InMemoryDeliveries, ConversationId("c1"))

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  protected def asking(by: Account): DeliveriesContract.Asking = {
    val c = ConversationId("c-asks")
    val kept =
      Conversation(c, Origin.Slack("T1", "C1", "1.0"), by, Instant.EPOCH, DeliveriesContract.Kept)
    val entries = new InMemoryEntryStore(id => Option.when(id == c)(kept))
    val principals = new InMemoryPrincipals
    val asked = new DeliveriesContract.Asking {
      val deliveries: Deliveries = new InMemoryDeliveries(entries, principals)
      val turn: TurnRef = TurnRef(c, TurnSeq.First)
      def append(id: EntryId, payload: Payload): Unit = {
        given Tx = TestTx.fake
        (for {
          next <- entries.lockNext(c)
          _ <- entries.insert(
            Entry(id, c, turn.turnSeq, None, next.seq, payload, Instant.EPOCH, Integrity.Member)
          )
        } yield ()).fold(e => throw new java.lang.AssertionError(e.toString), identity)
      }
      def at[A](clearance: Clearance)(body: (Tx^) ?=> A): A = body(using TestTx.fake(clearance))
    }
    asked.append(EntryId("first"), Payload.Message(Message.User("edit it")))
    principals.authored(EntryId("first"), by)
    asked
  }
}
