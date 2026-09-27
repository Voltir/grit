package grit.dbos.engine

import java.util.UUID

import grit.core.id.ConversationId
import grit.core.store.{ConversationContract, ConversationStore, Tx}
import grit.dbos.sql.{LiveDb, SqlConversationStore, TestPostgres}

/** The conversation contract, kept by the SQL store against a real Postgres. */
object SqlConversationTests extends ConversationContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_conversations")
    LiveEngine.open(c, "test").close()
    c
  }

  protected val conversations: ConversationStore = new SqlConversationStore()

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

  protected val unknown: ConversationId = ConversationId(UUID.randomUUID().toString)
}
