package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, PrincipalId}

/** An in-memory [[ConversationStore]] for tests, keeping [[ConversationContract]]. It ignores
  * the `Tx`; each new conversation's id is `c` and its number, from 1, never reused after a
  * removal, and it is created at the epoch. It holds conversations alone: a removal deletes
  * nothing of other stores.
  */
final class InMemoryConversationStore extends ConversationStore {

  @caps.unsafe.untrackedCaptures
  var all = Vector.empty[Conversation]

  @caps.unsafe.untrackedCaptures
  private var made = 0

  def findOrCreate(origin: Origin, by: PrincipalId)(using Tx^): Either[StoreError, Conversation] =
    all.find(_.origin == origin) match {
      case Some(found) => Right(found)
      case None =>
        made += 1
        val created = Conversation(ConversationId(s"c$made"), origin, by, Instant.EPOCH)
        all = all :+ created
        Right(created)
    }

  def find(origin: Origin)(using Tx^): Either[StoreError, Option[Conversation]] =
    Right(all.find(_.origin == origin))

  def get(id: ConversationId)(using Tx^): Either[StoreError, Option[Conversation]] =
    Right(all.find(_.id == id))

  def remove(conversation: ConversationId)(using Tx^): Either[StoreError, Unit] = {
    all = all.filterNot(_.id == conversation)
    Right(())
  }
}
