package grit.core.store

import java.time.Instant

import grit.core.id.{CallSlot, ConversationId}
import grit.core.identity.Account
import grit.core.visibility.Label

/** An in-memory [[ConversationStore]] for tests, keeping [[ConversationContract]]. It ignores
  * the `Tx`; each new conversation's id is `c` and its number, from 1, never reused after a
  * removal, and it is created at the epoch. A removal deletes the conversation's entries and
  * next positions in `entries`, when given; nothing of any other store (its periods and
  * verdicts are kept).
  */
final class InMemoryConversationStore(entries: Option[InMemoryEntryStore] = None)
    extends ConversationStore {

  @caps.unsafe.untrackedCaptures
  var all = Vector.empty[Conversation]

  @caps.unsafe.untrackedCaptures
  private var made = 0

  def findOrCreate(origin: Origin, by: Account, label: Label)(using
      Tx^
  ): Either[StoreError, Conversation] =
    all.find(_.origin == origin) match {
      case Some(found) => Right(found)
      case None =>
        made += 1
        val created = Conversation(ConversationId(s"c$made"), origin, by, Instant.EPOCH, label)
        all = all :+ created
        Right(created)
    }

  def find(origin: Origin)(using Tx^): Either[StoreError, Option[Conversation]] =
    Right(all.find(_.origin == origin))

  def get(id: ConversationId)(using Tx^): Either[StoreError, Option[Conversation]] =
    Right(all.find(_.id == id))

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
