package grit.core

/** Conversations, one per [[Origin]]. */
trait ConversationStore {

  /** The conversation for `origin`, creating it on first use. Calls with an
    * equal origin return the same conversation, including concurrent ones.
    */
  def findOrCreate(origin: Origin)(using Tx^): Either[StoreError, Conversation]
}
