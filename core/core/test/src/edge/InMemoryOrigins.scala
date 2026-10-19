package grit.core.edge

import grit.core.id.ConversationId
import grit.core.store.{InMemoryConversationStore, Origin, StoreError, Tx}
import grit.core.visibility.Item

/** An in-memory [[Origins]] for tests, keeping [[OriginsContract]], over the conversations
  * `conversations` holds, each read as an entry in its origin's room is.
  */
final class InMemoryOrigins(conversations: InMemoryConversationStore) extends Origins {

  def of(conversation: ConversationId)(using tx: Tx^): Either[StoreError, Option[Origin]] =
    Right(
      conversations.all
        .find(c =>
          c.id == conversation && Tx.clearance(tx).reads(Item.InRoom(c.origin.room), c.label)
        )
        .map(_.origin)
    )
}
