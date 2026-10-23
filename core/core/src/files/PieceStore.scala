package grit.core.files

import java.time.Instant

import grit.core.id.{PieceId, PieceSetId, PluginName}
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}

/** A file as its pieces are kept: `ref`, its keeping plugin's name for it (a Drive file's id);
  * `name`, as a person knows it; `revision`, its source's; read by `format` at `version`
  * ([[Understood.by]]).
  */
final case class PiecedFile(
    ref: String,
    name: String,
    revision: String,
    format: FormatName,
    version: Int
)

/** A piece as kept: by `plugin`, of `file`, at `place`, at `label` and believed at
  * `integrity`, written `written`.
  */
final case class KeptPiece(
    id: PieceId,
    plugin: PluginName,
    file: PiecedFile,
    place: Place,
    label: Label,
    integrity: Integrity,
    written: Instant,
    piece: Piece
)

/** Kept pieces as windows draw on them. Each read returns only the pieces its transaction reads
  * (a piece is kept and read as a document is, [[grit.core.document.DocumentShelf]]).
  */
trait PieceSearch {

  /** The pieces of `plugins`' files, current at `at`, that its transaction reads and that match
    * `query` with [[PieceIndex.query]]'s aliases added: best first, at most `limit`, at most
    * [[PieceSearch.PerFile]] of one file, each with its score, positive and higher better, on
    * the pieces index's own scale, and how many more of its file's pieces matched than it
    * answers, counted among the best `limit` × [[PieceSearch.PerFile]]. Empty when nothing
    * matches, `query` is blank, `limit` is not positive or `plugins` is empty.
    */
  def search(plugins: Vector[PluginName], query: String, limit: Int, at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[PieceSearch.Hit]]

  /** Of `plugins`, each with a piece current at `at` that its transaction reads, in their
    * order: those a [[search]] could find anything of.
    */
  def holding(plugins: Vector[PluginName], at: Instant)(using
      Tx^
  ): Either[StoreError, Vector[PluginName]]

  /** The pieces among `ids` still kept and read by its transaction, in their order. */
  def read(ids: Vector[PieceId])(using Tx^): Either[StoreError, Vector[KeptPiece]]
}

object PieceSearch {

  /** 8: the most pieces of one file a search answers. */
  val PerFile: Int = 8

  final case class Hit(piece: KeptPiece, score: Double, more: Int)
}

/** [[PieceSearch]], and what the engine itself writes of every plugin's pieces. */
trait PieceStore extends PieceSearch {

  /** Counts the files of `ids` placed by a window recorded at `at`: last placement `at` unless
    * later, what a keeper's bound reads ([[PieceKeeper.keep]]). One no longer kept is skipped.
    */
  def placed(ids: Vector[PieceId], at: Instant)(using Tx^): Either[StoreError, Unit]

  /** Deletes `set` with its pieces: [[read]] no longer finds them. Nothing when it is gone. */
  def forget(set: PieceSetId)(using Tx^): Either[StoreError, Unit]

  /** Deletes every piece of `plugin`'s, and ends at `at` the tombstones pending on its sets. */
  def remove(plugin: PluginName, at: Instant)(using Tx^): Either[StoreError, Unit]
}

/** One plugin's pieces as it writes them. */
trait PieceKeeper {

  /** `pieces` as the current pieces of `file`, at `place`, written `at`, `cut` more dropped
    * ([[Pieces.of]]): kept at the transaction's floor ([[grit.core.visibility.Clearance.floor]])
    * in its own room, or in none when it has none, as [[grit.core.document.DocumentKeeper.write]]
    * keeps a document; believed at `integrity`. The pieces kept before for `file`'s ref at that
    * label stop being current and are marked for deletion after the plugin's retention
    * ([[grit.core.retention.Target.PieceSet]]); one whose fingerprint is among the new keeps its
    * id. Nothing new is kept when those pieces were the same, in order, at the same place,
    * integrity, format and version: only `file`'s name and revision change. Then, while the
    * plugin's pieces kept, current or not, take more than the keeper's bound in bytes
    * ([[Piece.size]]), the files kept least recently placed are deleted whole, at any label:
    * those no longer current first, the oldest first, then the current ones, never `file`'s.
    * How many pieces are kept.
    */
  def keep(
      file: PiecedFile,
      place: Place,
      integrity: Integrity,
      pieces: Vector[Piece],
      cut: Int,
      at: Instant
  )(using Tx^): Either[StoreError, Int]

  /** `ref`'s pieces at `label` no longer current from `at`, and marked for deletion as [[keep]]
    * marks those it replaces; whether there were any.
    */
  def withdraw(ref: String, label: Label, at: Instant)(using Tx^): Either[StoreError, Boolean]
}

object PieceKeeper {

  /** 128 MiB: the bytes a plugin's pieces may take, unless its keeper is given another. */
  val Bound: Long = 128L * 1024 * 1024
}
