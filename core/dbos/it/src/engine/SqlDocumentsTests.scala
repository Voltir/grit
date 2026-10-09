package grit.dbos.engine

import grit.core.document.{
  DocumentContract,
  DocumentKeeper,
  DocumentSearch,
  DocumentShelf,
  DocumentStore,
  DocumentTerms
}
import grit.core.id.PluginName
import grit.core.store.{Origin, Savepoints, Tombstones, Tx}
import grit.core.visibility.Clearance
import grit.dbos.sql.{
  LiveDb,
  SqlDocumentSearch,
  SqlDocuments,
  SqlSavepoints,
  SqlTombstones,
  TestPostgres
}

/** The documents contract, kept by the SQL store against a real Postgres. */
object SqlDocumentsTests extends DocumentContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_documents")
    LiveEngine.open(c, "test").close()
    c
  }

  protected val tombstones: Tombstones = new SqlTombstones

  private val documents = new SqlDocuments(tombstones)

  protected val savepoints: Savepoints = SqlSavepoints

  protected val store: DocumentStore = documents

  // The read half over the read-only class an attached link is handed.
  override protected val search: DocumentSearch = new SqlDocumentSearch

  protected def keeper(plugin: PluginName, terms: DocumentTerms): DocumentKeeper =
    documents.keeper(plugin, terms)

  protected def shelf(plugin: PluginName): DocumentShelf = documents.shelf(plugin)

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    LiveDb.transaction(config, clearance)(body)

  protected def room(origin: Origin): Unit = {
    val _ = LiveDb.conversation(config, origin)
  }
}
