package grit.core.store

import java.time.Instant

import grit.core.id.{CallSlot, ConversationId}
import grit.core.identity.Account
import grit.core.visibility.Label

/** An in-memory [[ConversationStore]] for tests, keeping [[ConversationContract]]. It ignores
  * the `Tx`; each new conversation's id is `c` and its number, from 1, never reused after a
  * removal, and it is created at the epoch. A removal deletes the conversation's entries and
  * next positions in `entries`, when given; nothing of any other store (its periods and
  * verdicts are kept). A conversation in [[unreadable]] is found as a row the store cannot
  * read: `Invalid`, as SqlConversationStore reads a malformed one.
  */
final class InMemoryConversationStore(entries: Option[InMemoryEntryStore] = None)
    extends ConversationStore {

  @caps.unsafe.untrackedCaptures
  var all = Vector.empty[Conversation]

  @caps.unsafe.untrackedCaptures
  private var made = 0

  /** The conversations [[find]] and [[get]] read as `Invalid`. */
  @caps.unsafe.untrackedCaptures
  var unreadable = Set.empty[ConversationId]

  /** `found`, unless it is [[unreadable]]. */
  private def read(found: Option[Conversation]): Either[StoreError, Option[Conversation]] =
    found match {
      case Some(c) if unreadable(c.id) =>
        Left(StoreError.Invalid(s"conversation ${ConversationId.value(c.id)} is unreadable"))
      case other => Right(other)
    }

  def findOrCreate(origin: Origin, by: Account, label: Label)(using
      Tx^
  ): Either[StoreError, Conversation] =
    read(all.find(_.origin == origin)).flatMap {
      case Some(found) => Right(found)
      case None =>
        made += 1
        val created = Conversation(ConversationId(s"c$made"), origin, by, Instant.EPOCH, label)
        all = all :+ created
        Right(created)
    }

  def find(origin: Origin)(using Tx^): Either[StoreError, Option[Conversation]] =
    read(all.find(_.origin == origin))

  def get(id: ConversationId)(using Tx^): Either[StoreError, Option[Conversation]] =
    read(all.find(_.id == id))

  /** The call each conversation's opening post was made by, as an inbox keeps it. */
  @caps.unsafe.untrackedCaptures
  var posts = Map.empty[ConversationId, CallSlot]

  def postedBy(conversation: ConversationId)(using Tx^): Either[StoreError, Option[CallSlot]] =
    Right(posts.get(conversation))

  def remove(conversation: ConversationId)(using Tx^): Either[StoreError, Unit] = {
    all = all.filterNot(_.id == conversation)
    posts = posts - conversation
    entries.foreach(_.forget(conversation))
    Right(())
  }
}
