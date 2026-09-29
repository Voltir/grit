package grit.core.triage

import java.time.Instant

import grit.core.id.EntryId
import grit.core.store.{InMemoryEntryStore, StoreError, Tx}

/** An in-memory [[TriageStore]] for tests, keeping [[TriageContract]]: its tags are of the
  * entries `entries` still holds. It ignores the `Tx`.
  */
final class InMemoryTriageStore(entries: InMemoryEntryStore) extends TriageStore {

  @caps.unsafe.untrackedCaptures
  private var rows = Map.empty[EntryId, Tags]

  def record(entry: EntryId, tags: Tags, at: Instant)(using Tx^): Either[StoreError, Boolean] =
    entries.get(entry).map {
      case Some(_) if !rows.contains(entry) =>
        rows = rows.updated(entry, tags)
        true
      case _ => false
    }

  def of(ids: Vector[EntryId])(using Tx^): Either[StoreError, Map[EntryId, Tags]] =
    ids.foldLeft[Either[StoreError, Map[EntryId, Tags]]](Right(Map.empty)) { (acc, id) =>
      acc.flatMap(found =>
        entries.get(id).map {
          case Some(_) => rows.get(id).fold(found)(t => found.updated(id, t))
          case None => found
        }
      )
    }
}
