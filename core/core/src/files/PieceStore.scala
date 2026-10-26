package grit.core.files

import java.time.Instant

import grit.core.document.Pointer
import grit.core.id.{PieceId, PieceSetId, PluginName}
import grit.core.message.Tokens
import grit.core.place.Place
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}

/** A file as its pieces are kept: `ref`, its keeping plugin's name for it (a Drive file's id);
  * `name`, as a person knows it; `revision`, its source's; of `kind` as its source classes it
  * (a deck read as its PDF export is a [[Kind.GoogleSlides]]); read by `format` at `version`
  * ([[Understood.by]]); read again by the call `pointer` names; cut at `target` tokens a piece
  * ([[Pieces.Target]] when it was cut).
  */
final case class PiecedFile(
    ref: String,
    name: String,
    revision: String,
    kind: Kind,
    format: FormatName,
    version: Int,
    pointer: Pointer,
    target: Tokens
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
) {

  /** The call that reads around it: its file's pointer with [[RangeArgs.NearArgument]] its
    * anchor as written ([[Anchor.written]]); its file's pointer alone when that would be too
    * long a pointer ([[Pointer.of]]).
    */
  def pointer: Pointer =
    file.pointer
      .setting(RangeArgs.NearArgument, ujson.Str(piece.anchor.written))
      .getOrElse(file.pointer)
}

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

  /** The pieces of `plugin`'s file that `pointer` reads, at `revision` ([[PiecedFile.revision]]),
    * current at exactly `label`, in their file's order, at most `first`; of two such files (one
    * file kept under two refs), the one kept first. Empty when there is none, it was marked
    * unread, or its transaction does not read `label`.
    */
  def file(plugin: PluginName, pointer: Pointer, revision: String, label: Label, first: Int)(using
      Tx^
  ): Either[StoreError, Vector[KeptPiece]]
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
    * integrity, format, version and target: only `file`'s name, revision, kind and pointer
    * change. Then, while the plugin's pieces kept, current or not, take more than the keeper's
    * bound in bytes ([[Piece.size]]), the files kept least recently placed are deleted whole, at
    * any label: those no longer current first, the oldest first, then the current ones, never
    * `file`'s. How many pieces are kept.
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

  /** `ref`'s current pieces at `from` made its current pieces at the transaction's floor, at
    * `place`, written `at`, in its own room as [[keep]] keeps them: the same pieces, each keeping
    * its id, with the file's name, revision, format and when it was last placed, believed at the
    * meet of what they were and `most`. Those at `from`, and any current at the floor
    * before, stop being current and are marked for deletion as [[keep]] marks those it replaces.
    * A file marked [[unread]] at `from` is marked so at the floor. How many pieces are kept:
    * 0, keeping and withdrawing nothing, when `ref` has nothing current at `from`.
    */
  def relabel(ref: String, from: Label, place: Place, most: Integrity, at: Instant)(using
      Tx^
  ): Either[StoreError, Int]

  /** `ref`'s file, at `revision`, marked read with no pieces kept, `why`, at the transaction's
    * floor and `place`, written `at`: what it held there before stops being current and is
    * marked for deletion as [[keep]] marks what it replaces. [[kept]] answers it
    * [[KeptAs.Unread]] until a [[keep]], [[withdraw]] or another mark replaces it.
    */
  def unread(ref: String, revision: String, place: Place, why: String, at: Instant)(using
      Tx^
  ): Either[StoreError, Unit]

  /** Of `refs`, those with something current at the transaction's floor, each as what. */
  def kept(refs: Vector[String])(using Tx^): Either[StoreError, Map[String, KeptAs]]
}

/** What a file has current at a label ([[PieceKeeper.kept]]). */
enum KeptAs extends caps.Pure derives CanEqual {

  /** Its pieces, of a file of `kind` ([[PiecedFile.kind]]), read by `format` at `version`, cut at
    * `target` tokens a piece.
    */
  case Read(kind: Kind, format: FormatName, version: Int, target: Tokens)

  /** None: it was read and none kept, `why` ([[PieceKeeper.unread]]). */
  case Unread(why: String)
}

object PieceKeeper {

  /** 128 MiB: the bytes a plugin's pieces may take, unless its keeper is given another. */
  val Bound: Long = 128L * 1024 * 1024
}
