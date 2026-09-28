package grit.dbos.engine

import java.util.UUID

import grit.core.edge.{Deliveries, DeliveriesContract}
import grit.core.id.ConversationId
import grit.core.store.{Origin, Tx}
import grit.dbos.sql.{LiveDb, SqlDeliveries, TestPostgres}

/** The deliveries contract, kept by the SQL store against a real Postgres. Each test gets a
  * database of its own, since pending lists every awaited turn.
  */
object SqlDeliveriesTests extends DeliveriesContract {

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
}
