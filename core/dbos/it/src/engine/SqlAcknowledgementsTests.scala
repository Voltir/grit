package grit.dbos.engine

import java.util.UUID

import grit.core.edge.{Acknowledgements, AcknowledgementsContract}
import grit.core.id.ConversationId
import grit.core.store.{Origin, Tx}
import grit.dbos.sql.{LiveDb, SqlAcknowledgements, TestPostgres}

/** The acknowledgements contract, kept by the SQL store against a real Postgres. Each test gets a
  * database of its own, since standing lists every acknowledgement.
  */
object SqlAcknowledgementsTests extends AcknowledgementsContract {

  @caps.unsafe.untrackedCaptures
  private var config: Option[grit.dbos.sql.DbConfig] = None

  protected def fresh(): (Acknowledgements, ConversationId) = {
    val c =
      TestPostgres.freshDatabase(s"sql_acknowledgements_${UUID.randomUUID().toString.take(8)}")
    LiveEngine.open(c, "test").close()
    config = Some(c)
    (new SqlAcknowledgements(), LiveDb.conversation(c, Origin.Task("acknowledgements", "c")).id)
  }

  protected def transaction[A](body: (Tx^) ?=> A): A =
    LiveDb.transaction(config.getOrElse(throw new java.lang.AssertionError("no database")))(body)
}
