package grit.core.stitch

import java.time.Instant

import grit.core.id.ConversationId
import grit.core.message.Cut
import grit.core.store.{ConversationStore, StoreError, Tx}

/** The strands of the rooms a transaction reads ([[Strand.Digested]]). Each read takes each
  * strand's messages once, text and all, and keeps only their ids, count and lengths.
  */
trait StrandShelf {

  /** Each strand with at least one follower, a member of which had a message said from `from`
    * until before `until`, as it stood at `until`; oldest touched first.
    */
  def touched(from: Instant, until: Instant)(using Tx^): Either[StoreError, Vector[Strand.Digested]]

  /** [[touched]], only the strands whose room's label in force is the transaction's floor
    * ([[grit.core.store.Tx.floor]]): what a run kept at one label reads.
    */
  def atFloor(from: Instant, until: Instant)(using Tx^): Either[StoreError, Vector[Strand.Digested]]

  /** Each of `roots`' strands as it stood at `asOf`, in `roots`' order; a root none of whose
    * members holds a message kept and read is left out.
    */
  def of(roots: Vector[ConversationId], asOf: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Strand.Digested]]
}

object StrandShelf {

  /** The shelf over `stitches`, where strands are linked and said in, and `conversations`,
    * where each root's room is.
    */
  def over(stitches: StitchStore, conversations: ConversationStore): StrandShelf =
    new Over(stitches, conversations)

  /** What `root`'s strand said before `asOf`: the root's messages and its followers', oldest
    * first. The one read both a [[Strand.Digested]] and a [[StrandExcerpts]] read are made of.
    */
  private[stitch] def saidIn(stitches: StitchStore, root: ConversationId, asOf: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Said]] =
    stitches
      .links(Vector(root))
      .map(_.filter(_.root == root).map(_.conversation).distinct)
      .flatMap(followers => stitches.said(root +: followers, Instant.EPOCH, asOf))

  private final class Over(stitches: StitchStore, conversations: ConversationStore)
      extends StrandShelf {

    def touched(from: Instant, until: Instant)(using
        Tx^
    ): Either[StoreError, Vector[Strand.Digested]] =
      stitches
        .touched(from, until)
        .flatMap(links => of(links.map(_.root).distinct, until))
        .map(_.sortBy(_.touched))

    def atFloor(from: Instant, until: Instant)(using
        tx: Tx^
    ): Either[StoreError, Vector[Strand.Digested]] =
      touched(from, until).map(_.filter(_.roomLabel == Tx.floor(tx)))

    def of(roots: Vector[ConversationId], asOf: Instant)(using
        Tx^
    ): Either[StoreError, Vector[Strand.Digested]] =
      roots.foldLeft[Either[StoreError, Vector[Strand.Digested]]](Right(Vector.empty)) { (acc, root) =>
        acc.flatMap(done => seen(root, asOf).map(done ++ _))
      }

    private def seen(root: ConversationId, asOf: Instant)(using
        Tx^
    ): Either[StoreError, Option[Strand.Digested]] =
      for {
        conversation <- conversations.get(root)
        said <- saidIn(stitches, root, asOf)
      } yield
        for {
          c <- conversation
          first <- said.headOption
          last <- said.lastOption
        } yield {
          val room = c.origin.room
          val heard = said.map(_.conversation).distinct
          Strand.Digested(
            room,
            Tx.roomLabel(room),
            root,
            heard.filter(_ == root) ++ heard.filterNot(_ == root),
            said.size,
            said.flatMap(Stitching.spoken).map(Cut.head(_, Stitching.MessageChars).length).sum,
            first.entry.createdAt,
            last.entry.createdAt,
            StrandDigest.of(said.map(_.entry.id)),
            asOf
          )
        }
  }
}
