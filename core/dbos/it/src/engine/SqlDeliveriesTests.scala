package grit.dbos.engine

import java.time.Instant
import java.util.UUID

import grit.core.edge.{Deliveries, DeliveriesContract}
import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.identity.Account
import grit.core.store.{Entry, Origin, Payload, Tx}
import grit.core.visibility.{Clearance, Integrity, TestLabels}
import grit.dbos.sql.{LiveDb, SqlDeliveries, SqlEntryStore, TestPostgres}

/** The deliveries contract, kept by the SQL store against a real Postgres. Each test gets a
  * database of its own, since pending lists every awaited turn.
  */
object SqlDeliveriesTests extends DeliveriesContract {

  // Set once, by the suite's setup, to an immutable config; read only by its tests.
  @caps.unsafe.untrackedCaptures
  private var config: Option[grit.dbos.sql.DbConfig] = None

  protected def fresh(): (Deliveries, ConversationId) = {
    val c = TestPostgres.freshDatabase(s"sql_deliveries_${UUID.randomUUID().toString.take(8)}")
    LiveEngine.open(c, "test").close()
    config = Some(c)
    (new SqlDeliveries(), LiveDb.conversation(c, Origin.Task("deliveries", "c")).id)
  }

  protected def transaction[A](body: (Tx^) ?=> A): A =
    LiveDb.transaction(config.getOrElse(throw new java.lang.AssertionError("no database")))(body)

  /** Over a database of its own, its engine opened once under [[TestLabels.Trialled]], which
    * declares the compartment [[DeliveriesContract.Kept]] names.
    */
  protected def asking(by: Account): DeliveriesContract.Asking = {
    val c = TestPostgres.freshDatabase(s"sql_asks_${UUID.randomUUID().toString.take(8)}")
    LiveEngine.open(c, "test", visibility = TestLabels.Trialled).close()
    val conversation =
      LiveDb.conversation(c, Origin.Slack("T1", "C1", "1.0"), DeliveriesContract.Kept)
    val asked = TurnRef(conversation.id, TurnSeq.First)
    LiveDb.asking(c, asked, by, None)
    new DeliveriesContract.Asking {
      val deliveries: Deliveries = new SqlDeliveries()
      val turn: TurnRef = asked
      def append(id: EntryId, payload: Payload): Unit = {
        val entries = new SqlEntryStore()
        LiveDb
          .transaction(c)(for {
            next <- entries.lockNext(asked.conversationId)
            _ <- entries.insert(
              Entry(
                id,
                asked.conversationId,
                asked.turnSeq,
                None,
                next.seq,
                payload,
                Instant.EPOCH,
                Integrity.Member
              )
            )
          } yield ())
          .fold(e => throw new java.lang.AssertionError(e.toString), identity)
      }
      def at[A](clearance: Clearance)(body: (Tx^) ?=> A): A = LiveDb.transaction(c, clearance)(body)
    }
  }
}
