package grit.core.triage

import java.time.Instant

import grit.core.id.EntryId
import grit.core.store.{StoreError, Tx}

/** What triage made of each heard message, kept beside its entry and deleted with it, so a
  * period's raw purge takes its tags.
  */
trait TriageStore {

  /** Keeps `tags` for `entry`, made `at`; `false`, writing nothing, when the entry is gone or
    * already tagged.
    */
  def record(entry: EntryId, tags: Tags, at: Instant)(using Tx^): Either[StoreError, Boolean]

  /** The tags kept for each of `entries` that has any. */
  def of(entries: Vector[EntryId])(using Tx^): Either[StoreError, Map[EntryId, Tags]]
}
