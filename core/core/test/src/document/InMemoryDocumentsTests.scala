package grit.core.document

import grit.core.id.{DocumentVersionId, PluginName}
import grit.core.review.DocumentReviews
import grit.core.store.{Origin, Savepoints, Tombstones, Tx}
import grit.core.visibility.{Clearance, DeclassificationPolicy, Label, Visibility}
import grit.dbos.sql.TestTx

/** The documents contract, kept by the in-memory fake. */
object InMemoryDocumentsTests extends DocumentContract {

  private val documents = new InMemoryDocuments

  protected val store: DocumentStore = documents

  protected def keeper(plugin: PluginName, terms: DocumentTerms): DocumentKeeper =
    documents.keeper(plugin, terms)

  protected def shelf(plugin: PluginName): DocumentShelf = documents.shelf(plugin)

  protected val tombstones: Tombstones = documents.tombstones

  protected val savepoints: Savepoints = documents.savepoints

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    body(using TestTx.fake(clearance))

  protected val reviews: DocumentReviews = documents.reviews

  protected def openedUnder[A](clearance: Clearance, visibility: Visibility)(body: (Tx^) ?=> A): A =
    body(using TestTx.fake(clearance, visibility))

  // Rooms are places, which the fake needs no row for.
  protected def room(origin: Origin): Unit = ()

  protected def policyAt(
      over: DeclassificationPolicy.Over,
      version: Int,
      text: Option[String],
      kept: Label,
      checks: Option[ujson.Value]
  ): Unit = documents.policy(over, version, text, kept, checks)

  protected def application(
      pointer: Pointer,
      version: Int,
      over: Option[DeclassificationPolicy.Over]
  ): Unit = documents.application(pointer, version, over)

  protected def audited(
      v: DocumentVersionId
  ): Option[(source: DocumentVersionId, policy: Int, application: Int)] =
    documents.declassification(v)
}
