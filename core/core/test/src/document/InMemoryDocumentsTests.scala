package grit.core.document

import grit.core.id.PluginName
import grit.core.store.{Tombstones, Tx}
import grit.dbos.sql.TestTx

/** The documents contract, kept by the in-memory fake. */
object InMemoryDocumentsTests extends DocumentContract {

  private val documents = new InMemoryDocuments

  protected val store: DocumentStore = documents

  protected def keeper(plugin: PluginName, terms: DocumentTerms): DocumentKeeper =
    documents.keeper(plugin, terms)

  protected def shelf(plugin: PluginName): DocumentShelf = documents.shelf(plugin)

  protected val tombstones: Tombstones = documents.tombstones

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
}
