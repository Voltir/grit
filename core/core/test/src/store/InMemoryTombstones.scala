package grit.core.store

import java.time.Instant

import grit.core.id.{DocumentVersion, PluginName}
import grit.core.retention.{Target, Tombstone}

/** An in-memory [[Tombstones]] for tests, keeping [[TombstonesContract]]. It ignores the `Tx`.
  * `owner` says which plugin's a document version is, as its row would; `None` for one with
  * no row.
  */
final class InMemoryTombstones(owner: DocumentVersion -> Option[PluginName] = _ => None)
    extends Tombstones {
  import InMemoryTombstones.Row

  @caps.unsafe.untrackedCaptures
  private var rows = Map.empty[Target, Row]

  /** Every pending tombstone, oldest written first, then in the collector's order of kinds:
    * what a test reads to see what was written.
    */
  def pending: Vector[Tombstone] =
    rows.toVector
      .collect { case (t, r) if r.ended.isEmpty => Tombstone(t, r.written) }
      .sortBy(t => (t.written, t.target.kind.ordinal, Target.key(t.target)))

  def write(target: Target, at: Instant)(using Tx^): Either[StoreError, Boolean] =
    rows.get(target) match {
      case Some(r) if r.ended.isEmpty => Right(false)
      case _ =>
        rows = rows.updated(target, Row(at, None, None, spared = false))
        Right(true)
    }

  def spare(target: Target, at: Instant)(using Tx^): Either[StoreError, Unit] =
    end(target, at, spared = true)

  def due(kind: Target.Kind, before: Instant, n: Int)(using
      Tx^
  ): Either[StoreError, Vector[Tombstone]] =
    Right(
      rows.toVector
        .collect {
          case (t, r) if t.kind == kind && r.ended.isEmpty && r.written.isBefore(before) =>
            (r.deferred.getOrElse(r.written), Target.key(t), Tombstone(t, r.written))
        }
        .sortBy((at, key, _) => (at, key))
        .take(n max 0)
        .map(_._3)
    )

  def documentsDue(plugin: PluginName, before: Instant, n: Int)(using
      Tx^
  ): Either[StoreError, Vector[Tombstone]] =
    due(Target.Kind.Document, before, Int.MaxValue).map(
      _.filter(_.target match {
        case Target.Document(v) => owner(v).contains(plugin)
        case _ => false
      }).take(n max 0)
    )

  def deferred(target: Target, at: Instant)(using Tx^): Either[StoreError, Unit] = {
    rows.get(target) match {
      case Some(r) if r.ended.isEmpty => rows = rows.updated(target, r.copy(deferred = Some(at)))
      case _ => ()
    }
    Right(())
  }

  def collected(target: Target, at: Instant)(using Tx^): Either[StoreError, Unit] =
    end(target, at, spared = false)

  def forget(before: Instant)(using Tx^): Either[StoreError, Int] = {
    val gone = rows.collect { case (t, r) if r.ended.exists(_.isBefore(before)) => t }
    rows = rows -- gone
    Right(gone.size)
  }

  private def end(target: Target, at: Instant, spared: Boolean): Either[StoreError, Unit] = {
    rows.get(target) match {
      case Some(r) if r.ended.isEmpty =>
        rows = rows.updated(target, r.copy(ended = Some(at), spared = spared))
      case _ => ()
    }
    Right(())
  }
}

object InMemoryTombstones {
  private final case class Row(
      written: Instant,
      ended: Option[Instant],
      deferred: Option[Instant],
      spared: Boolean
  )
}
