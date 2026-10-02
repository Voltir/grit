package grit.core.triage

import java.time.Instant

import grit.core.id.{EntryId, TriageRef}
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

  /** Every heard message tagged at or after `from` and before `until`, oldest first: its
    * entry, its triage, when, and the tags.
    */
  def tagged(from: Instant, until: Instant)(using
      Tx^
  ): Either[StoreError, Vector[TriageStore.Tagged]]
}

object TriageStore {

  /** One heard message's tags: its `entry`, its `triage`, and when (`at`) they were made. */
  final case class Tagged(entry: EntryId, triage: TriageRef, at: Instant, tags: Tags)
}
