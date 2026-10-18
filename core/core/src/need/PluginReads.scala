package grit.core.need

import grit.core.document.DocumentShelf
import grit.core.store.{StoreError, Tx}

/** One plugin's own documents as its tools and its service read them: its cache documents of
  * its cursor's generation, and its current documents.
  */
final case class PluginReads(cache: PluginDocs, documents: DocumentShelf)

/** One plugin's documents as its surfaces read them, only those its transaction reads (as
  * [[grit.core.document.DocumentShelf]] reads documents). No other plugin's are reachable.
  */
trait PluginDocs {

  /** The document under `key`, or `None` when there is none or its transaction does not read
    * it.
    */
  def get(key: String)(using Tx^): Either[StoreError, Option[ujson.Value]]

  /** Its documents whose keys start with `prefix` that its transaction reads, greatest key
    * first, at most `n`.
    */
  def newest(prefix: String, n: Int)(using Tx^): Either[StoreError, Vector[(String, ujson.Value)]]
}
