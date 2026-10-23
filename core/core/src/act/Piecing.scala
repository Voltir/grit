package grit.core.act

import java.time.Instant

import grit.core.files.{PieceKeeper, PiecedFile, Pieces, Understood}
import grit.core.id.FileHandle
import grit.core.place.Place
import grit.core.provider.TokenEstimator
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.{Integrity, Label}

/** What a piece move does ([[Keeping.piece]]). A file is named by `ref`, unique among its
  * plugin's files, such as a card's key.
  */
enum Piecing extends caps.Pure derives CanEqual {

  /** Keep the pieces of the file the area keeps under `file` as `ref`'s current pieces at
    * `revision`, drawn on at `place`; first withdrawing `ref`'s pieces at each of `replacing`.
    */
  case Read(file: FileHandle, ref: String, revision: String, place: Place, replacing: Vector[Label])

  /** Withdraw each file's pieces at its label: `ref` to that label. */
  case Withdraw(gone: Vector[(String, Label)])
}

/** What a piece move came to. */
enum Pieced extends caps.Pure derives CanEqual {

  /** `pieces` kept, `cut` more past [[grit.core.files.Pieces.PerFile]] dropped. */
  case Kept(pieces: Int, cut: Int)

  /** None kept, since the file is gone or no format reads it, `why` in the area's words: its
    * `ref`'s pieces at the floor are withdrawn too, so none of an earlier revision stays.
    */
  case Unread(why: String)

  /** The pieces of `files` of the files named withdrawn; the others had none. */
  case Withdrawn(files: Int)
}

object Piecing {

  /** What `read` comes to over `keeper`, written `at`, in this transaction: its file `file` (the
    * name its sender gave it and its tree, or why it is unread), cut by [[Pieces.of]] under
    * `estimate` and kept at the floor believed at `integrity`, or, unread, `read.ref`'s pieces
    * withdrawn there; and its pieces at each of `read.replacing` but the floor withdrawn first.
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
        withdrawn(replaced :+ (read.ref, floor), keeper, at).map(_ => Pieced.Unread(why))
      case Right((name, tree)) =>
        val (pieces, cut) = Pieces.of(tree, estimate)
        val kept = PiecedFile(read.ref, name, read.revision, tree.by, tree.version)
        for {
          _ <- withdrawn(replaced, keeper, at)
          n <- keeper.keep(kept, read.place, integrity, pieces, cut, at)
        } yield Pieced.Kept(n, cut)
    }
  }

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
