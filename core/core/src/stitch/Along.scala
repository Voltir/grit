package grit.core.stitch

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.id.ConversationId
import grit.core.store.{Conversation, ConversationStore, Entry, StoreError, Tx}

/** The one way a reader reads its conversation's strand. */
object Along {

  /** What a reader read of a conversation's strand: the conversation, and the strand. */
  final case class Read(conversation: Option[Conversation], strand: Strand.Read)

  /** What `conversation`'s strand said, read from `horizon` before `all`'s first entry (`until`
    * when `all` is empty) until before `until`; with the conversation. An empty strand, and no
    * conversation, for one not found.
    */
  def read(
      stitches: StitchStore,
      conversations: ConversationStore,
      conversation: ConversationId,
      all: Vector[Entry],
      horizon: FiniteDuration,
      until: Instant
  )(using Tx^): Either[StoreError, Read] =
    for {
      found <- conversations.get(conversation)
      strand <- found.fold[Either[StoreError, Strand.Read]](Right(Strand.Read.empty)) { c =>
        val began = all.minByOption(_.seq).fold(until)(_.createdAt)
        read(stitches, c, began.minusNanos(horizon.toNanos), until)
      }
    } yield Read(found, strand)

  /** What the other conversations of `conversation`'s strand said from `from` until before
    * `until`, oldest first, and its root's opening message however old, read through
    * `stitches`: only conversations within its room (ADR 0023's stitch rule). A member whose
    * first message is gone is named in `gone`, for its record to be shown instead; every
    * member, in its room or not, in `conversations`. Empty for a conversation that is not
    * stitchable.
    */
  def read(
      stitches: StitchStore,
      conversation: Conversation,
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
        def held(s: Said) = s.place.within(room)
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
