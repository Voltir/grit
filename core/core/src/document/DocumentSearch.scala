package grit.core.document

import java.time.Instant

import grit.core.id.{DocumentVersion, PluginName}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}

/** A place where `plugin` keeps a current document. */
final case class Shelved(plugin: PluginName, place: Place)

/** The enabled plugins' documents as windows draw on them. Read-only. */
trait DocumentSearch {

  /** Each enabled plugin's terms, as the newest engine start declared them. */
  def declared()(using Tx^): Either[StoreError, Vector[(PluginName, DocumentTerms)]]

  /** Each place where one of `plugins` kept a document current at `at` (written before `at`,
    * superseded by none written before `at`, not a withdrawal), once per plugin and place.
    */
  def shelved(plugins: Vector[PluginName], at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Shelved]]

  /** The documents at `shelves`, current at `at`, that match `query`: best first, at most
    * `limit`, equally good matches latest written first, each with its score, positive and
    * higher better, on the document index's own scale, not [[grit.core.store.EntrySearch]]'s.
    * Empty when nothing matches, `query` is blank, `limit` is not positive or `shelves` is
    * empty.
    */
  def search(shelves: Vector[Shelved], query: String, limit: Int, at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[DocumentSearch.Hit]]

  /** The versions among `versions` still kept and holding something, in their order. */
  def read(versions: Vector[DocumentVersion])(using Tx^): Either[StoreError, Vector[Document]]
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
  def placed(versions: Vector[DocumentVersion], at: Instant)(using Tx^): Either[StoreError, Unit]

  /** The plugins with document rows kept, enabled or not. */
  def kept()(using Tx^): Either[StoreError, Vector[PluginName]]
}
