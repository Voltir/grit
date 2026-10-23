package grit.core.act

import java.time.Instant

import grit.core.document.Pointer
import grit.core.files.{FormatName, KeptAs, PieceKeeper, PiecedFile, Pieces, Understood}
import grit.core.id.FileHandle
import grit.core.message.Tokens
import grit.core.place.Place
import grit.core.provider.TokenEstimator
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}

/** What a piece move does ([[Keeping.piece]]). A file is named by `ref`, unique among its
  * plugin's files, such as a card's key.
  */
enum Piecing extends caps.Pure derives CanEqual {

  /** Keep the pieces of the file the area keeps under `file` as `ref`'s current pieces at
    * `revision`, read again by `pointer`, drawn on at `place`; first withdrawing `ref`'s pieces
    * at each of `replacing`.
    */
  case Read(
      file: FileHandle,
      ref: String,
      revision: String,
      pointer: Pointer,
      place: Place,
      replacing: Vector[Label]
  )

  /** Withdraw each file's pieces at its label: `ref` to that label. */
  case Withdraw(gone: Vector[(String, Label)])

  /** Move `ref`'s current pieces at `from` to the floor, drawn on at `place`, as its card is
    * moved ([[grit.core.files.PieceKeeper.relabel]]).
    */
  case Relabel(ref: String, from: Label, place: Place)

  /** Mark `ref`'s file, at `revision`, read with none of its pieces kept, `why`, drawn on at
    * `place`, so it is not read again for them until it changes
    * ([[grit.core.files.PieceKeeper.unread]]); first withdrawing `ref`'s pieces at each of
    * `replacing`.
    */
  case Mark(ref: String, revision: String, place: Place, why: String, replacing: Vector[Label])

  /** Of the files `refs`, find those whose pieces are due: none kept at the floor, kept by a
    * format the area no longer reads at that version, or cut at another target than
    * [[Pieces.Target]]. One marked unread is not due.
    */
  case Survey(refs: Vector[String])
}

/** What a piece move came to. */
enum Pieced extends caps.Pure derives CanEqual {

  /** `pieces` kept, `cut` more past [[grit.core.files.Pieces.PerFile]] dropped. */
  case Kept(pieces: Int, cut: Int)

  /** None kept, `why`: the file is gone or no format reads it, in the area's words, or a
    * [[Piecing.Mark]] said why. The file is marked so at the floor, replacing what it held
    * there, so none of an earlier revision stays.
    */
  case Unread(why: String)

  /** The pieces of `files` of the files named withdrawn; the others had none. */
  case Withdrawn(files: Int)

  /** `pieces` moved to the floor: none when the file had none at the label it was moved from. */
  case Relabelled(pieces: Int)

  /** Of the files a [[Piecing.Survey]] named, those whose pieces are due, in its order. */
  case Due(refs: Vector[String])
}

object Piecing {

  /** What `read` comes to over `keeper`, written `at`, in this transaction: its file `file` (the
    * name its sender gave it and its tree, or why it is unread), cut by [[Pieces.of]] under
    * `estimate` and kept at the floor believed at `integrity`, or, unread, `read.ref` marked so
    * there; and its pieces at each of `read.replacing` but the floor withdrawn first.
    * The one meaning of a read, every [[Keeping]]'s.
    */
  def kept(
      read: Read,
      file: Either[String, (String, Understood)],
      keeper: PieceKeeper,
      integrity: Integrity,
      estimate: TokenEstimator,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Pieced] = {
    val floor = Tx.floor(tx)
    val replaced = read.replacing.filter(_ != floor).map(l => (read.ref, l))
    file match {
      case Left(why) =>
        marked(Mark(read.ref, read.revision, read.place, why, read.replacing), keeper, at)
      case Right((name, tree)) =>
        val (pieces, cut) = Pieces.of(tree, estimate)
        val kept =
          PiecedFile(
            read.ref,
            name,
            read.revision,
            tree.by,
            tree.version,
            read.pointer,
            Pieces.Target
          )
        for {
          _ <- withdrawn(replaced, keeper, at)
          n <- keeper.keep(kept, read.place, integrity, pieces, cut, at)
        } yield Pieced.Kept(n, cut)
    }
  }

  /** What `relabel` comes to over `keeper`, written `at`, in this transaction, its pieces
    * believed at no more than `integrity`. The one meaning of a relabel, every [[Keeping]]'s.
    */
  def relabelled(relabel: Relabel, keeper: PieceKeeper, integrity: Integrity, at: Instant)(using
      Tx^
  ): Either[StoreError, Pieced] =
    keeper
      .relabel(relabel.ref, relabel.from, relabel.place, integrity, at)
      .map(Pieced.Relabelled(_))

  /** What `mark` comes to over `keeper`, written `at`, in this transaction: `mark.ref` marked
    * unread at the floor, its pieces at each of `mark.replacing` but the floor withdrawn first.
    * The one meaning of a mark, every [[Keeping]]'s.
    */
  def marked(mark: Mark, keeper: PieceKeeper, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Pieced] = {
    val floor = Tx.floor(tx)
    for {
      _ <- withdrawn(mark.replacing.filter(_ != floor).map(l => (mark.ref, l)), keeper, at)
      _ <- keeper.unread(mark.ref, mark.revision, mark.place, mark.why, at)
    } yield Pieced.Unread(mark.why)
  }

  /** What `survey` comes to over `keeper`, in this transaction, a file kept by a format at a
    * version due unless `reads` says the area reads with it, and one cut at another target than
    * [[Pieces.Target]] due. The one meaning of a survey,
    * every [[Keeping]]'s.
    */
  def due(survey: Survey, keeper: PieceKeeper, reads: (FormatName, Int) -> Boolean)(using
      Tx^
  ): Either[StoreError, Pieced] =
    keeper
      .kept(survey.refs)
      .map(held =>
        Pieced.Due(survey.refs.filter { ref =>
          held.get(ref) match {
            case None => true
            case Some(KeptAs.Read(format, version, target)) =>
              !reads(format, version) || Tokens.value(target) != Tokens.value(Pieces.Target)
            case Some(KeptAs.Unread(_)) => false
          }
        })
      )

  /** How many of `gone`'s files had pieces at their labels, each withdrawn through `keeper`,
    * `at`, in this transaction.
    */
  def withdrawn(gone: Vector[(String, Label)], keeper: PieceKeeper, at: Instant)(using
      Tx^
  ): Either[StoreError, Int] =
    gone.foldLeft[Either[StoreError, Int]](Right(0)) { (acc, g) =>
      acc.flatMap(n => keeper.withdraw(g._1, g._2, at).map(w => if (w) n + 1 else n))
    }
}
