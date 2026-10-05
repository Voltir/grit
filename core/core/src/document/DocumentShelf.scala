package grit.core.document

import java.time.Instant

import grit.core.id.{DocKey, DocumentVersion}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}

/** One plugin's documents as it reads them: the current ones, withdrawals left out. No other
  * plugin's are reachable.
  */
trait DocumentShelf {

  /** The current document under `key`; `None` when it has none or was withdrawn. */
  def current(key: DocKey)(using Tx^): Either[StoreError, Option[Document]]

  /** Its current documents, most recently written first, at most `n`; none when `n` is not
    * positive.
    */
  def newest(n: Int)(using Tx^): Either[StoreError, Vector[Document]]
}

/** One plugin's documents as it writes them, under the terms it declared. */
trait DocumentKeeper extends DocumentShelf {

  /** Makes `text` and `data`, at `place`, the current document under `key`, written `at`; the
    * version before, if any, stops being current and is marked for deletion
    * ([[grit.core.retention.Target.Document]]). [[Written.Unchanged]] when the current version
    * holds the same place, text and data. Past the bound, the current documents placed least
    * recently (ties by key) are withdrawn as [[withdraw]] does, never the one written. A
    * `DatabaseError` when another transaction wrote `key` meanwhile; posting never does, as it
    * runs one at a time per plugin.
    */
  def write(key: DocKey, place: Place, text: DocText, data: ujson.Value, at: Instant)(using
      Tx^
  ): Either[StoreError, Written]

  /** Withdraws `key`'s current document at `at`: a version holding nothing becomes current,
    * and both are marked for deletion. The withdrawal's version, or `None` when `key` had no
    * current document.
    */
  def withdraw(key: DocKey, at: Instant)(using Tx^): Either[StoreError, Option[DocumentVersion]]
}
