package grit.core.stitch

import grit.core.store.{Principals, StoreError, Tx}
import grit.core.visibility.Integrity

/** What a strand's members said, as a summary of it is asked of them: the one read of a
  * strand's text for a dreaming run, made in core's own step on its behalf
  * ([[grit.core.act.Dreaming.summarise]]), never handed to a job's code.
  */
trait StrandExcerpts {

  /** What `seen`'s members said before its `asOf` that the transaction reads, as
    * [[Stitching.excerpt]] shows them within `chars` (the root's first message, then the
    * latest), believed at the meet of every message read; `None` when those messages are not
    * the ones `seen` digested (one was removed or said since).
    */
  def excerpt(seen: Strand.Digested, chars: Int)(using Tx^): Either[StoreError, Option[Strand.Shown]]
}

object StrandExcerpts {

  /** The excerpts over `stitches`, where strands are linked and said in, and `principals`,
    * who said each message.
    */
  def over(stitches: StitchStore, principals: Principals): StrandExcerpts =
    new Over(stitches, principals)

  private final class Over(stitches: StitchStore, principals: Principals) extends StrandExcerpts {

    def excerpt(seen: Strand.Digested, chars: Int)(using
        Tx^
    ): Either[StoreError, Option[Strand.Shown]] =
      StrandShelf.saidIn(stitches, seen.root, seen.asOf).flatMap { said =>
        said.headOption.filter(_ => StrandDigest.of(said.map(_.entry.id)) == seen.digest) match {
          case None => Right(None)
          case Some(first) =>
            principals.speakers(said.map(_.entry.id)).map { speakers =>
              val opening = said.find(_.conversation == seen.root)
              Some(
                Strand.Shown(
                  Stitching.excerpt(opening, said, speakers, chars),
                  said.map(_.entry.integrity).foldLeft(first.entry.integrity)(_.meet(_))
                )
              )
            }
        }
      }
  }
}
