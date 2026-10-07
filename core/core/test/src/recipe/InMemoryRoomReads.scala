package grit.core.recipe

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, PrincipalId}
import grit.core.identity.TestAccounts
import grit.core.message.Message
import grit.core.place.Place
import grit.core.stitch.Said
import grit.core.store.{
  Entry,
  InMemoryEntryStore,
  InMemoryPrincipals,
  Origin,
  Payload,
  StoreError,
  Tx
}

/** An in-memory [[RoomReads]] for tests, keeping [[RoomReadsContract]] and
  * [[grit.core.store.ClearanceContract]], over the entries `entries` holds, which decides what a
  * transaction reads, each conversation at `origin`'s place, and the authors `principals` was
  * told. Otherwise it ignores the `Tx`.
  */
final class InMemoryRoomReads(
    entries: InMemoryEntryStore,
    origin: ConversationId -> Origin,
    principals: InMemoryPrincipals
) extends RoomReads {

  def said(room: Place, from: Instant, until: Instant, outside: Set[ConversationId], most: Int)(
      using Tx^
  ): Either[StoreError, Vector[Said]] =
    Right(latest(room, from, until, outside, most, _ => true))

  def saidBy(
      room: Place,
      author: PrincipalId,
      from: Instant,
      until: Instant,
      outside: Set[ConversationId],
      most: Int
  )(using Tx^): Either[StoreError, Vector[Said]] =
    Right(
      latest(
        room,
        from,
        until,
        outside,
        most,
        e => principals.author(e.id).map(TestAccounts.principalId).contains(author)
      )
    )

  def author(entry: EntryId)(using Tx^): Either[StoreError, Option[PrincipalId]] =
    Right(
      principals
        .author(entry)
        .map(TestAccounts.principalId)
        .filter(_ => entries.everything.exists(_.id == entry))
    )

  private def latest(
      room: Place,
      from: Instant,
      until: Instant,
      outside: Set[ConversationId],
      most: Int,
      by: Entry => Boolean
  )(using Tx^): Vector[Said] =
    entries.readable
      .filter(e =>
        !e.createdAt.isBefore(from) && e.createdAt.isBefore(until) &&
          !outside.contains(e.conversationId) && message(e) && by(e)
      )
      .map(e => Said(e.conversationId, origin(e.conversationId).place, e))
      .filter(_.place.within(room))
      .sortBy(s => (s.entry.createdAt, EntryId.value(s.entry.id)))
      .reverse
      .take(math.max(0, most))

  private def message(e: Entry): Boolean = e.payload match {
    case Payload.Heard(_) | Payload.Posted(_) | Payload.Message(Message.User(_)) => true
    case Payload.Message(Message.Assistant(_, _, _, _, _)) => true
    case _ => false
  }
}
