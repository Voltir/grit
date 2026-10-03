package grit.core.stitch

import java.time.Instant

import grit.core.place.Scope
import grit.core.store.{Conversation, StoreError, Tx}

/** The one way a reader reads its conversation's strand. */
object Along {

  /** What the other conversations of `conversation`'s strand said from `from` until before
    * `until`, oldest first, and its root's opening message however old, read through
    * `stitches`: only conversations at places `scope` holds for its room. A member whose
    * first message is gone is named in `gone`, for its record to be shown instead; every
    * member, in scope or not, in `conversations`. Empty for a conversation that is not
    * stitchable.
    */
  def read(
      stitches: StitchStore,
      conversation: Conversation,
      scope: Scope,
      from: Instant,
      until: Instant
  )(using Tx^): Either[StoreError, Strand.Read] =
    if (!conversation.origin.stitchable) Right(Strand.Read.empty)
    else {
      val c = conversation.id
      val room = conversation.origin.room
      for {
        own <- stitches.links(Vector(c))
        root = own.find(_.conversation == c).fold(c)(_.root)
        theirs <- if (root == c) Right(own) else stitches.links(Vector(root))
        members = (Strand.of(c, own ++ theirs) - c).toVector.sortBy(_.toString)
        openings <- stitches.openings(members)
        said <- stitches.said(members, from, until)
      } yield {
        def held(s: Said) = s.place.within(room) && scope.holds(room, s.place)
        val kept = openings.filter(held)
        Strand.Read(
          kept.find(_.conversation == root),
          said.filter(held),
          members.filterNot(m => openings.exists(_.conversation == m)),
          members.toSet
        )
      }
    }
}
