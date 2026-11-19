package grit.core.stitch

import grit.core.store.{ConversationStore, Principals, StoreError, Tx}
import grit.core.visibility.Integrity

/** What a strand's members said, as a summary of it is asked of them: the one read of a
  * strand's text for a dreaming run, made in core's own step on its behalf
  * ([[grit.core.act.Dreaming.summarise]]), never handed to a job's code.
  */
trait StrandExcerpts {

  /** What `seen`'s members said before its `asOf` that the transaction reads, as
    * [[Stitching.excerpt]] shows them within `chars` (its opening, a thread's asking message or
    * else the root's first message, then the latest; a run read by its span, with context
    * marked apart), believed at the meet of every message read, the opening and context
    * included; `None` when the messages it places are not the ones `seen` digested (one was
    * removed or said since).
    */
  def excerpt(seen: Strand.Digested, chars: Int)(using
      Tx^
  ): Either[StoreError, Option[Strand.Shown]]
}

object StrandExcerpts {

  /** The excerpts over `stitches`, where strands are linked and said in, `conversations`, where
    * a thread's asking message is found, and `principals`, who said each message.
    */
  def over(
      stitches: StitchStore,
      conversations: ConversationStore,
      principals: Principals
  ): StrandExcerpts =
    new Over(stitches, conversations, principals)

  private final class Over(
      stitches: StitchStore,
      conversations: ConversationStore,
      principals: Principals
  ) extends StrandExcerpts {

    def excerpt(seen: Strand.Digested, chars: Int)(using
        Tx^
    ): Either[StoreError, Option[Strand.Shown]] =
      StrandShelf.placing(stitches, conversations, seen).flatMap { p =>
        val said = p.placed
        said.headOption.filter(_ => StrandDigest.of(said.map(_.entry.id)) == seen.digest) match {
          case None => Right(None)
          case Some(first) =>
            val read = (p.opening.toVector ++ p.before ++ said ++ p.after).distinctBy(_.entry.id)
            principals.speakers(read.map(_.entry.id)).map { speakers =>
              Some(
                Strand.Shown(
                  Stitching.excerpt(p.opening, said, speakers, chars, p.before, p.after),
                  read.map(_.entry.integrity).foldLeft(first.entry.integrity)(_.meet(_))
                )
              )
            }
        }
      }
  }
}
