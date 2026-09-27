package grit.dbos.engine

import java.util.UUID

import grit.core.id.ConversationId
import grit.core.store.{
  EntryStore,
  ModelFactStore,
  ModelProfileStore,
  Origin,
  PromptStore,
  StoreContract,
  Tx,
  UsageLedger
}
import grit.core.tool.ToolSets
import grit.dbos.sql.{
  LiveDb,
  SqlEntryStore,
  SqlModelFactStore,
  SqlModelProfileStore,
  SqlPromptStore,
  SqlToolSets,
  SqlUsageLedger,
  TestPostgres
}

/** The store contract, kept by the SQL stores against a real Postgres. */
object SqlStoreTests extends StoreContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_store")
    Engine.open(c, "test").close()
    c
  }

  protected val entries: EntryStore = new SqlEntryStore()
  protected val ledger: UsageLedger = new SqlUsageLedger()
  protected val profiles: ModelProfileStore = new SqlModelProfileStore()
  protected val facts: ModelFactStore = new SqlModelFactStore()
  protected val toolSets: ToolSets = new SqlToolSets()
  protected val prompts: PromptStore = new SqlPromptStore()

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)

  protected def conversation(name: String): ConversationId =
    LiveDb.conversation(config, Origin.Task("contract", name)).id

  protected val unknownConversation: ConversationId = ConversationId(UUID.randomUUID().toString)
}
