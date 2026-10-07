package grit.core.stitch

import java.time.Instant

import grit.core.id.{ConversationId, EntryId}
import grit.core.message.Message
import grit.core.place.Place
import grit.core.store.{Entry, InMemoryEntryStore, Origin, Payload, StoreError, Tx}

/** An in-memory [[StitchStore]] for tests, keeping [[StitchContract]] and
  * [[grit.core.store.ClearanceContract]]: its placements are of the entries `entries` still
  * holds, which decides what a transaction reads, and each conversation is at `origin`'s place.
  * Otherwise it ignores the `Tx`.
  */
final class InMemoryStitchStore(entries: InMemoryEntryStore, origin: ConversationId -> Origin)
    extends StitchStore {

  @caps.unsafe.untrackedCaptures
  private var rows = Vector.empty[(EntryId, ConversationId, Placed)]

  def record(root: EntryId, placed: Placed, at: Instant)(using
      Tx^
  ): Either[StoreError, Boolean] =
    entries.get(root).flatMap {
      case None => Right(false)
      case Some(e) =>
        val first = entries.everything
          .filter(_.conversationId == e.conversationId)
          .minByOption(_.seq)
          .exists(_.id == root)
        if (!first)
          Left(StoreError.Invalid(s"${EntryId.value(root)} is not its conversation's first entry"))
        else if (rows.exists(_._1 == root)) Right(false)
        else {
          rows = rows :+ ((root, e.conversationId, placed))
          Right(true)
        }
    }

  def placed(root: EntryId)(using Tx^): Either[StoreError, Option[Placed]] =
    Right(kept.collectFirst { case (id, _, p) if id == root => p })

  def spokenIn(room: Place, from: Instant, until: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Said]] =
    Right(
      inRange(from, until)
        .filter(e => message(e) || closing(e))
        .map(said)
        .filter(_.place.within(room))
    )

  def said(conversations: Vector[ConversationId], from: Instant, until: Instant)(using
      Tx^
  ): Either[StoreError, Vector[Said]] =
    Right(
      inRange(from, until)
        .filter(e => conversations.contains(e.conversationId) && message(e))
        .map(said)
    )

  def openings(conversations: Vector[ConversationId])(using Tx^): Either[StoreError, Vector[Said]] =
    Right(
      conversations.flatMap(c =>
        entries.everything
          .filter(_.conversationId == c)
          .minByOption(_.seq)
          .filter(e => message(e) && entries.readable.contains(e))
          .map(said)
      )
    )

  def links(conversations: Vector[ConversationId])(using Tx^): Either[StoreError, Vector[Link]] =
    Right(
      kept
        .flatMap((_, c, p) => Placed.link(c, p))
        .filter(l => conversations.contains(l.conversation) || conversations.contains(l.root))
    )

  /** The rows whose root entry is still kept. */
  private def kept: Vector[(EntryId, ConversationId, Placed)] =
    rows.filter((id, _, _) => entries.everything.exists(_.id == id))

  /** The entries the transaction reads made in [`from`, `until`), oldest first. */
  private def inRange(from: Instant, until: Instant)(using Tx^): Vector[Entry] =
    entries.readable
      .filter(e => !e.createdAt.isBefore(from) && e.createdAt.isBefore(until))
      .sortBy(e => (e.createdAt, e.seq))

  private def said(e: Entry): Said = Said(e.conversationId, origin(e.conversationId).place, e)

  private def message(e: Entry): Boolean = e.payload match {
    case Payload.Heard(_) | Payload.Posted(_) | Payload.Message(Message.User(_)) => true
    case Payload.Message(Message.Assistant(_, _, _, _, _)) => true
    case _ => false
  }

  private def closing(e: Entry): Boolean = e.payload match {
    case Payload.Closed(_, _, _) => true
    case _ => false
  }
}
