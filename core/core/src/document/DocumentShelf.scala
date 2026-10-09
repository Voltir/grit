package grit.core.document

import java.time.Instant

import grit.core.id.{DocKey, DocumentVersion}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label

/** One plugin's documents as it reads them: the current ones, withdrawals left out, and only
  * those its transaction reads ([[grit.core.store.Tx.clearance]]): a document kept in the
  * transaction's own room up to that room's label, any other up to
  * [[grit.core.store.Tx.cleared]]. No other plugin's are reachable.
  */
trait DocumentShelf {

  /** The current document under `key` kept at exactly `label`; `None` when there is none there,
    * it was withdrawn, or this transaction does not read it: one kept above what it reads is
    * never returned, whatever is kept, so choosing a variant wrongly cannot read up.
    */
  def current(key: DocKey, label: Label)(using Tx^): Either[StoreError, Option[Document]]

  /** Its current documents this transaction reads, every key's and label's, most recently
    * written first, at most `n`; none when `n` is not positive.
    */
  def newest(n: Int)(using Tx^): Either[StoreError, Vector[Document]]
}

/** One plugin's documents as it writes them, under the terms it declared. */
trait DocumentKeeper extends DocumentShelf {

  /** Makes `text` and `data`, at `place`, the current document under `key` at `label` joined
    * with its transaction's floor ([[grit.core.visibility.Clearance.floor]]), written `at`:
    * never below what its writer could read. It is kept in the transaction's own room, or in
    * none when it has none, never a room the writer names. [[Written.kept]] says which label it
    * was kept at, and `current(key, kept)` finds it again. Under posting and a job's run the
    * floor is [[grit.core.store.Tx.cleared]], so that is `label join Tx.cleared(tx)`; a
    * person's turn may write above what it reads back. The version before under that key and
    * label, if any, stops being current and is marked for deletion
    * ([[grit.core.retention.Target.Document]]); a document under the key at another label is
    * untouched. [[Written.Unchanged]] when the current version there holds the same place, text
    * and data. Past the bound, counted over every label, the current documents placed least
    * recently (ties by key, then label in its written form, bytewise) are withdrawn as
    * [[withdraw]] does, never the one written: a high variant few readers place tends to go
    * first. A `DatabaseError` when another transaction wrote `key` at that label meanwhile:
    * posting never does, as it runs one at a time per plugin, but two runs of a plugin's
    * keeping jobs can, each keeping in its own transaction.
    */
  def write(key: DocKey, label: Label, place: Place, text: DocText, data: ujson.Value, at: Instant)(
      using Tx^
  ): Either[StoreError, Written]

  /** Withdraws the current document under `key` at exactly `label`, at `at`: a version holding
    * nothing, kept at that label and in that document's room, becomes current there, and both
    * are marked for deletion. The withdrawal's version, or `None` when `key` had no current
    * document at `label`.
    */
  def withdraw(key: DocKey, label: Label, at: Instant)(using
      Tx^
  ): Either[StoreError, Option[DocumentVersion]]
}
