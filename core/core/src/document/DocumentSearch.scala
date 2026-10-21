package grit.core.document

import java.time.Instant

import grit.core.id.{DocumentVersionId, PluginName}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}

/** A place where `plugin` keeps a current document. */
final case class Shelved(plugin: PluginName, place: Place)

/** The enabled plugins' documents as windows draw on them. Read-only. Each read returns only the
  * documents its transaction reads ([[DocumentShelf]]).
  */
trait DocumentSearch {

  /** Each enabled plugin's terms, as the newest engine start declared them. */
  def declared()(using Tx^): Either[StoreError, Vector[(PluginName, DocumentTerms)]]

  /** Each place where one of `plugins` kept a document current at `at` (written before `at`,
    * superseded by none written before `at`, not a withdrawal) that its transaction reads, once
    * per plugin and place.
    */
  def shelved(plugins: Vector[PluginName], at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Shelved]]

  /** The documents at `shelves`, current at `at`, that its transaction reads and that match
    * `query` (one it does not read never takes a readable one's place): best first, at most
    * `limit`, equally good matches latest written first, each with its score, positive and
    * higher better, on the document index's own scale, not [[grit.core.store.EntrySearch]]'s.
    * Empty when nothing matches, `query` is blank, `limit` is not positive or `shelves` is
    * empty.
    */
  def search(shelves: Vector[Shelved], query: String, limit: Int, at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[DocumentSearch.Hit]]

  /** The documents of every enabled plugin, current at `at`, that its transaction reads and that
    * match `text`, each with its plugin's label: best first, at most `limit`, as [[search]]
    * ranks them. Empty when `limit` is not positive or nothing matches.
    */
  final def nearby(text: String, limit: Int, at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[(DocumentSearch.Hit, DocLabel)]] =
    for {
      shelves <- shelvesAt(at)
      hits <-
        if (shelves.isEmpty) Right(Vector.empty) else search(shelves.map(_._1), text, limit, at)
      labels = shelves.map((s, t) => s.plugin -> t.label).toMap
    } yield hits.flatMap(h => labels.get(h.document.plugin).map(h -> _))

  /** Each place where an enabled plugin keeps a document current at `at` that the transaction
    * reads ([[shelved]] over [[declared]]), with that plugin's terms.
    */
  final def shelvesAt(at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[(Shelved, DocumentTerms)]] =
    for {
      plugins <- declared()
      shelves <-
        if (plugins.isEmpty) Right(Vector.empty) else shelved(plugins.map(_._1), at)
      terms = plugins.toMap
    } yield shelves.flatMap(s => terms.get(s.plugin).map(s -> _))

  /** The versions among `versions` still kept, holding something and read by its transaction,
    * in their order.
    */
  def read(versions: Vector[DocumentVersionId])(using Tx^): Either[StoreError, Vector[Document]]

  /** The versions among `versions` still kept whose plugin is enabled, in their order, each
    * with its plugin's label: what a window holding `versions` shows.
    */
  final def labelled(versions: Vector[DocumentVersionId])(using
      Tx^
  ): Either[StoreError, Vector[(Document, DocLabel)]] =
    for {
      kept <- read(versions)
      // The terms in force are read only when a document is left to label.
      terms <- if (kept.isEmpty) Right(Vector.empty) else declared()
      labels = terms.toMap
    } yield kept.flatMap(d => labels.get(d.plugin).map(t => d -> t.label))
}

object DocumentSearch {
  final case class Hit(document: Document, score: Double)
}

/** [[DocumentSearch]], and what the engine itself writes of every plugin's documents. */
trait DocumentStore extends DocumentSearch {

  /** Records `enabled` as the terms in force; one declared before and not among them is no
    * longer enabled: [[declared]] leaves it out, and the sweep marks it for deletion.
    */
  def declare(enabled: Vector[(PluginName, DocumentTerms)])(using Tx^): Either[StoreError, Unit]

  /** Counts each of `versions` placed by a window recorded at `at`: count up by one, last
    * placement `at` unless later. One no longer kept is skipped.
    */
  def placed(versions: Vector[DocumentVersionId], at: Instant)(using Tx^): Either[StoreError, Unit]

  /** Counts each of `versions` followed by a turn whose reply or draft was appended at `at`:
    * count up by one, last follow `at` unless later. One no longer kept, or that its transaction
    * does not read, is skipped.
    */
  def followed(versions: Vector[DocumentVersionId], at: Instant)(using
      Tx^
  ): Either[StoreError, Unit]

  /** The plugins with documents or terms kept, enabled or not. */
  def kept()(using Tx^): Either[StoreError, Vector[PluginName]]

  /** Deletes `version`: [[read]] no longer finds it. Nothing when it is gone already. */
  def forget(version: DocumentVersionId)(using Tx^): Either[StoreError, Unit]

  /** Deletes every version of `plugin`'s documents and its terms, and ends at `at` the
    * tombstones pending on those versions, so none is left naming a version gone.
    */
  def remove(plugin: PluginName, at: Instant)(using Tx^): Either[StoreError, Unit]
}
