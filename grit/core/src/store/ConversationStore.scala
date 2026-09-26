package grit.core.store

import grit.core.id.ConversationId

/** Conversations, one per [[Origin]]. */
trait ConversationStore {

  /** The conversation for `origin`, creating it on first use. Calls with an
    * equal origin return the same conversation, including concurrent ones.
    */
  def findOrCreate(origin: Origin)(using Tx^): Either[StoreError, Conversation]

  /** Deletes `conversation` with its entries, periods and verdicts, and its place when no
    * other conversation is there; nothing when it is gone already. Its usage, profiles and
    * documents are the caller's to delete.
    */
  def remove(conversation: ConversationId)(using Tx^): Either[StoreError, Unit]
}
