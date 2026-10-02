package grit.core.triage

import java.time.Instant

import grit.core.id.{EntryId, TriageRef, TurnRef}
import grit.core.store.{InMemoryEntryStore, PeriodStore, StoreError, Tx}

/** An in-memory [[TriageStore]] for tests, keeping [[TriageContract]]: its tags are of the
  * entries `entries` still holds, each in the period `periods` puts its turn in. It ignores
  * the `Tx`.
  */
final class InMemoryTriageStore(entries: InMemoryEntryStore, periods: PeriodStore)
    extends TriageStore {

  @caps.unsafe.untrackedCaptures
  private var rows = Map.empty[EntryId, (Tags, Instant)]

  def record(entry: EntryId, tags: Tags, at: Instant)(using Tx^): Either[StoreError, Boolean] =
    entries.get(entry).map {
      case Some(_) if !rows.contains(entry) =>
        rows = rows.updated(entry, (tags, at))
        true
      case _ => false
    }

  def of(ids: Vector[EntryId])(using Tx^): Either[StoreError, Map[EntryId, Tags]] =
    ids.foldLeft[Either[StoreError, Map[EntryId, Tags]]](Right(Map.empty)) { (acc, id) =>
      acc.flatMap(found =>
        entries.get(id).map {
          case Some(_) => rows.get(id).fold(found)((t, _) => found.updated(id, t))
          case None => found
        }
      )
    }

  def tagged(from: Instant, until: Instant)(using
      Tx^
  ): Either[StoreError, Vector[TriageStore.Tagged]] =
    rows.toVector
      .filter((_, row) => !row._2.isBefore(from) && row._2.isBefore(until))
      .sortBy((id, row) => (row._2, EntryId.value(id)))
      .foldLeft[Either[StoreError, Vector[TriageStore.Tagged]]](Right(Vector.empty)) {
        case (acc, (id, (tags, at))) =>
          acc.flatMap(found =>
            entries.get(id).flatMap {
              case None => Right(found)
              case Some(e) =>
                periods
                  .of(TurnRef(e.conversationId, e.turnSeq))
                  .map(
                    _.fold(found)(p =>
                      found :+ TriageStore.Tagged(id, TriageRef(p.ref, e.turnSeq), at, tags)
                    )
                  )
            }
          )
      }
}
