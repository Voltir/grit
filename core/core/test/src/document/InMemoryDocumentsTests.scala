package grit.core.document

import grit.core.id.PluginName
import grit.core.store.{Origin, Tombstones, Tx}
import grit.core.visibility.Clearance
import grit.dbos.sql.TestTx

/** The documents contract, kept by the in-memory fake. */
object InMemoryDocumentsTests extends DocumentContract {

  private val documents = new InMemoryDocuments

  protected val store: DocumentStore = documents

  protected def keeper(plugin: PluginName, terms: DocumentTerms): DocumentKeeper =
    documents.keeper(plugin, terms)

  protected def shelf(plugin: PluginName): DocumentShelf = documents.shelf(plugin)

  protected val tombstones: Tombstones = documents.tombstones

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    body(using TestTx.fake(clearance))

  // Rooms are places, which the fake needs no row for.
  protected def room(origin: Origin): Unit = ()
}
