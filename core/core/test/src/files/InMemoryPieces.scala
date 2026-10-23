package grit.core.files

import java.time.Instant

import grit.core.document.Pointer
import grit.core.id.{PieceId, PieceSetId, PluginName}
import grit.core.place.Place
import grit.core.retention.Target
import grit.core.store.{InMemoryTombstones, StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.{Integrity, Item, Label}

/** An in-memory [[PieceStore]], with each plugin's [[PieceKeeper]], for tests, keeping
  * [[PieceContract]]. Ids are numbered by one counter for pieces and one for sets, across every
  * plugin. It marks sets in `tombstones`, which reads whose a set is from it. Search scores a
  * piece by how often the query's words occur in its search text ([[PieceIndex.of]]), not by
  * BM25: the contract's searches order strictly under either. It floors and filters by the
  * `Tx`'s clearance, and otherwise ignores it.
  */
final class InMemoryPieces extends PieceStore {
  import InMemoryPieces.{PieceRow, SetRow}

  // Only ever replaced by a new immutable map, as the store's tables would be.
  @caps.unsafe.untrackedCaptures
  private var sets = Map.empty[Long, SetRow]

  // As `sets`.
  @caps.unsafe.untrackedCaptures
  private var rows = Map.empty[Long, PieceRow]

  // The identity columns' last values: only ever counted up.
  @caps.unsafe.untrackedCaptures
  private var lastSet = 0L

  // As `lastSet`.
  @caps.unsafe.untrackedCaptures
  private var lastPiece = 0L

  /** Where its keepers mark the sets they leave for deletion. */
  val tombstones: InMemoryTombstones = new InMemoryTombstones({
    case Target.PieceSet(s) => sets.get(PieceSetId.value(s)).map(_.plugin)
    case _ => None
  })

  /** `plugin`'s pieces as it writes them, bounded at `bound` bytes. */
  def keeper(plugin: PluginName, bound: Long): PieceKeeper = new PieceKeeper {
    def keep(
        file: PiecedFile,
        place: Place,
        integrity: Integrity,
        pieces: Vector[Piece],
        cut: Int,
        at: Instant
    )(using tx: Tx^): Either[StoreError, Int] = {
      val clearance = Tx.clearance(tx)
      val label = clearance.floor
      val before = current(plugin, file.ref, label)
      before match {
        case Some((s, r))
            if r.place == place && r.integrity == integrity && r.file.format == file.format &&
              r.file.version == file.version && r.file.target == file.target &&
              of(s).map(_._2.piece) == pieces =>
          sets = sets.updated(
            s,
            r.copy(file =
              r.file.copy(
                name = file.name,
                revision = file.revision,
                kind = file.kind,
                pointer = file.pointer
              )
            )
          )
          Right(pieces.size)
        case _ =>
          for {
            _ <- before.fold(Right(()))((s, r) => supersede(s, r, at))
            set = {
              lastSet += 1
              sets = sets.updated(
                lastSet,
                SetRow(
                  plugin,
                  file,
                  place,
                  label,
                  clearance.own.map(_.room),
                  integrity,
                  at,
                  None,
                  at,
                  cut
                )
              )
              lastSet
            }
            // Each old piece whose fingerprint is among the new, once, keeps its id.
            olds = before.fold(Vector.empty[(Long, PieceRow)])((s, _) => of(s))
            _ = pieces.zipWithIndex.foldLeft(olds) { (left, pi) =>
              val (p, i) = pi
              val row = PieceRow(set, i, p, PieceIndex.of(file.name, p), Piece.size(p, file.name))
              left.indexWhere(_._2.piece.fingerprint == p.fingerprint) match {
                case -1 =>
                  lastPiece += 1
                  rows = rows.updated(lastPiece, row)
                  left
                case k =>
                  rows = rows.updated(left(k)._1, row)
                  left.patch(k, Nil, 1)
              }
            }
            _ <- bounded(plugin, bound, set, at)
          } yield pieces.size
      }
    }

    def withdraw(ref: String, label: Label, at: Instant)(using Tx^): Either[StoreError, Boolean] =
      current(plugin, ref, label) match {
        case Some((s, r)) => supersede(s, r, at).map(_ => true)
        case None => Right(false)
      }

    def relabel(ref: String, from: Label, place: Place, most: Integrity, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Int] = {
      val clearance = Tx.clearance(tx)
      val floor = clearance.floor
      current(plugin, ref, from) match {
        case None => Right(0)
        case Some((s, r)) =>
          for {
            _ <- supersede(s, r, at)
            _ <- current(plugin, ref, floor).fold(Right(()))((f, fr) => supersede(f, fr, at))
          } yield {
            lastSet += 1
            val moved = r.copy(
              place = place,
              label = floor,
              room = clearance.own.map(_.room),
              integrity = r.integrity.meet(most),
              written = at,
              superseded = None
            )
            sets = sets.updated(lastSet, moved)
            val pieces = of(s)
            pieces.foreach((id, p) => rows = rows.updated(id, p.copy(set = lastSet)))
            pieces.size
          }
      }
    }

    def unread(ref: String, revision: String, place: Place, why: String, at: Instant)(using
        tx: Tx^
    ): Either[StoreError, Unit] = {
      val clearance = Tx.clearance(tx)
      val label = clearance.floor
      current(plugin, ref, label).fold(Right(()))((s, r) => supersede(s, r, at)).map { _ =>
        lastSet += 1
        // An unread set holds no pieces, so no read answers its file's pointer, as SQL keeps none.
        val none = Pointer
          .of(ToolName("none"), "ref" -> ujson.Str(ref))
          .fold(why => throw new IllegalStateException(why), identity)
        val file =
          PiecedFile(ref, ref, revision, Kind.Unknown, FormatName(""), 0, none, Pieces.Target)
        val row = SetRow(
          plugin,
          file,
          place,
          label,
          clearance.own.map(_.room),
          Integrity.Untrusted,
          at,
          None,
          at,
          0,
          Some(why)
        )
        sets = sets.updated(lastSet, row)
      }
    }

    def kept(refs: Vector[String])(using tx: Tx^): Either[StoreError, Map[String, KeptAs]] = {
      val floor = Tx.clearance(tx).floor
      Right(refs.flatMap { ref =>
        current(plugin, ref, floor).map((_, r) =>
          ref -> r.unread.fold(
            KeptAs.Read(r.file.kind, r.file.format, r.file.version, r.file.target)
          )(KeptAs.Unread(_))
        )
      }.toMap)
    }
  }

  /** `plugin`'s current set for `ref` at `label`. */
  private def current(plugin: PluginName, ref: String, label: Label): Option[(Long, SetRow)] =
    sets.find((_, r) =>
      r.plugin == plugin && r.file.ref == ref && r.label == label && r.superseded.isEmpty
    )

  /** Set `s`'s pieces, by id, in order. */
  private def of(s: Long): Vector[(Long, PieceRow)] =
    rows.toVector.filter(_._2.set == s).sortBy(_._2.ordinal)

  private def supersede(s: Long, r: SetRow, at: Instant)(using Tx^): Either[StoreError, Unit] = {
    sets = sets.updated(s, r.copy(superseded = Some(at)))
    tombstones.write(Target.PieceSet(set(s)), at).map(_ => ())
  }

  /** `plugin`'s sets deleted, but `kept`, while they take more than `bound` bytes: those
    * superseded first, oldest first, then current ones least recently placed.
    */
  private def bounded(plugin: PluginName, bound: Long, kept: Long, at: Instant)(using
      Tx^
  ): Either[StoreError, Unit] = {
    val mine = sets.toVector.filter(_._2.plugin == plugin)
    val total = mine.map((s, _) => of(s).map(_._2.bytes).sum).sum
    val order = mine
      .filter(_._1 != kept)
      .sortBy((s, r) =>
        (
          r.superseded.isEmpty,
          r.superseded.getOrElse(r.lastPlaced).toEpochMilli,
          r.file.ref,
          s
        )
      )
    var left = total
    val gone = order.map(_._1).takeWhile { s =>
      val over = left > bound
      left -= of(s).map(_._2.bytes).sum
      over
    }
    gone.foldLeft[Either[StoreError, Unit]](Right(())) { (acc, s) =>
      acc.flatMap(_ =>
        forget(set(s)).flatMap(_ => tombstones.collected(Target.PieceSet(set(s)), at))
      )
    }
  }

  private def reads(r: SetRow)(using tx: Tx^): Boolean =
    Tx.clearance(tx).reads(Item.Kept(r.room), r.label)

  private def currentAt(r: SetRow, at: Instant): Boolean =
    r.written.isBefore(at) && r.superseded.forall(!_.isBefore(at))

  private def kept(id: Long, p: PieceRow): Option[KeptPiece] =
    for {
      r <- sets.get(p.set)
      pid <- PieceId.of(id)
    } yield KeptPiece(pid, r.plugin, r.file, r.place, r.label, r.integrity, r.written, p.piece)

  def search(plugins: Vector[PluginName], query: String, limit: Int, at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[PieceSearch.Hit]] = {
    val words = InMemoryPieces.words(PieceIndex.query(query)).distinct
    val scored = rows.toVector
      .flatMap { (id, p) =>
        sets
          .get(p.set)
          .filter(r => plugins.contains(r.plugin) && currentAt(r, at) && reads(r))
          .map { _ =>
            val found = InMemoryPieces.words(p.search)
            (id, p, words.map(w => found.count(_ == w)).sum.toDouble)
          }
      }
      .filter(_._3 > 0)
      .sortBy((id, _, score) => (-score, id))
      .take((limit max 0) * PieceSearch.PerFile)
    val counts = scored.groupBy(_._2.set).view.mapValues(_.size).toMap
    val answered = scored
      .groupBy(_._2.set)
      .values
      .flatMap(_.take(PieceSearch.PerFile))
      .toVector
      .sortBy((id, _, score) => (-score, id))
      .take(limit max 0)
    val shown = answered.groupBy(_._2.set).view.mapValues(_.size).toMap
    Right(answered.flatMap { (id, p, score) =>
      kept(id, p).map(
        PieceSearch.Hit(_, score, counts.getOrElse(p.set, 0) - shown.getOrElse(p.set, 0))
      )
    })
  }

  def holding(plugins: Vector[PluginName], at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[PluginName]] =
    Right(plugins.filter { p =>
      sets.exists((s, r) => r.plugin == p && currentAt(r, at) && reads(r) && of(s).nonEmpty)
    })

  /** `plugin`'s current pieces its transaction reads, by file, then in order: what a test
    * reads back of a keep.
    */
  def current(plugin: PluginName)(using Tx^): Vector[KeptPiece] =
    sets.toVector
      .filter((_, r) => r.plugin == plugin && r.superseded.isEmpty && reads(r))
      .sortBy((s, r) => (r.file.ref, s))
      .flatMap((s, _) => of(s).flatMap((id, p) => kept(id, p)))

  /** When the file holding piece `id` was last placed, while it is kept: what a test reads back
    * of a window's placements.
    */
  def lastPlaced(id: PieceId): Option[Instant] =
    rows.get(PieceId.value(id)).flatMap(p => sets.get(p.set)).map(_.lastPlaced)

  def read(ids: Vector[PieceId])(using Tx^): Either[StoreError, Vector[KeptPiece]] =
    Right(ids.flatMap { i =>
      val id = PieceId.value(i)
      rows.get(id).filter(p => sets.get(p.set).exists(reads)).flatMap(kept(id, _))
    })

  def placed(ids: Vector[PieceId], at: Instant)(using Tx^): Either[StoreError, Unit] = {
    ids.flatMap(i => rows.get(PieceId.value(i))).map(_.set).distinct.foreach { s =>
      sets.get(s).foreach { r =>
        sets =
          sets.updated(s, r.copy(lastPlaced = if (at.isAfter(r.lastPlaced)) at else r.lastPlaced))
      }
    }
    Right(())
  }

  def forget(set: PieceSetId)(using Tx^): Either[StoreError, Unit] = {
    val s = PieceSetId.value(set)
    sets = sets - s
    rows = rows.filter(_._2.set != s)
    Right(())
  }

  def remove(plugin: PluginName, at: Instant)(using Tx^): Either[StoreError, Unit] =
    sets.toVector.filter(_._2.plugin == plugin).foldLeft[Either[StoreError, Unit]](Right(())) {
      (acc, sr) =>
        acc.flatMap(_ =>
          tombstones.collected(Target.PieceSet(set(sr._1)), at).flatMap(_ => forget(set(sr._1)))
        )
    }

  private def set(s: Long): PieceSetId =
    PieceSetId.of(s).getOrElse(throw new IllegalStateException(s"set $s"))
}

object InMemoryPieces {

  /** A set's row: `label` and `room` as it is kept. */
  private final case class SetRow(
      plugin: PluginName,
      file: PiecedFile,
      place: Place,
      label: Label,
      room: Option[Place],
      integrity: Integrity,
      written: Instant,
      superseded: Option[Instant],
      lastPlaced: Instant,
      cut: Int,
      unread: Option[String] = None
  )

  /** A piece's row, in set `set` at `ordinal`. */
  private final case class PieceRow(
      set: Long,
      ordinal: Int,
      piece: Piece,
      search: String,
      bytes: Long
  )

  /** `text`'s words, lower-cased: its runs of letters and digits. */
  private def words(text: String): Vector[String] =
    """[\p{L}\d]+""".r.findAllIn(text.toLowerCase).toVector
}
