package grit.core.triage

import java.time.Instant

import grit.core.id.{EntryId, ShadowName, TriageRef}
import grit.core.store.{InMemoryEntryStore, StoreError, Tx}

/** An in-memory [[TriageShadows]] for tests, keeping [[TriageShadowsContract]]: its rows are
  * of the entries `entries` still holds, and the messages it offers are those `triage` has
  * tagged. It ignores the `Tx`.
  */
final class InMemoryTriageShadows(entries: InMemoryEntryStore, triage: TriageStore)
    extends TriageShadows {

  @caps.unsafe.untrackedCaptures
  private var rows = Map.empty[(EntryId, ShadowName), (Shadowed, Instant)]

  def record(entry: EntryId, name: ShadowName, row: Shadowed, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean] =
    entries.get(entry).map {
      case Some(_) if !rows.contains((entry, name)) =>
        rows = rows.updated((entry, name), (row, at))
        true
      case _ => false
    }

  def unshadowed(name: ShadowName, since: Instant, limit: Int)(using
      Tx^
  ): Either[StoreError, Vector[TriageRef]] =
    triage
      .tagged(since, Instant.MAX)
      .map(_.filterNot(t => rows.contains((t.entry, name))).take(limit).map(_.triage))

  def spent(name: ShadowName, from: Instant)(using Tx^): Either[StoreError, BigDecimal] =
    kept(name).map(_.filter((_, _, at) => !at.isBefore(from)).flatMap((_, row, _) => cost(row)).sum)

  def recent(name: ShadowName, n: Int)(using Tx^): Either[StoreError, Vector[BigDecimal]] =
    kept(name).map(_.sortBy((_, _, at) => at).reverse.flatMap((_, row, _) => cost(row)).take(n))

  def of(name: ShadowName, ids: Vector[EntryId])(using
      Tx^
  ): Either[StoreError, Map[EntryId, Shadowed]] =
    kept(name).map(_.collect { case (id, row, _) if ids.contains(id) => id -> row }.toMap)

  /** `name`'s rows whose entries are still there. */
  private def kept(
      name: ShadowName
  )(using Tx^): Either[StoreError, Vector[(EntryId, Shadowed, Instant)]] =
    rows.toVector
      .collect { case ((id, n), (row, at)) if n == name => (id, row, at) }
      .foldLeft[Either[StoreError, Vector[(EntryId, Shadowed, Instant)]]](Right(Vector.empty)) {
        (acc, r) => acc.flatMap(found => entries.get(r._1).map(_.fold(found)(_ => found :+ r)))
      }

  private def cost(row: Shadowed): Option[BigDecimal] = row match {
    case Shadowed.Answered(_, _, usage, _, _, _) => usage.costUsd
    case Shadowed.Failed(_, _, _) => None
  }
}
