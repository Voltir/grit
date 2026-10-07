package grit.core.store

import grit.core.id.{CallSlot, ConversationId, PrincipalId}
import grit.core.visibility.Label

/** Conversations, one per [[Origin]]. */
trait ConversationStore {

  /** The conversation for `origin`, created on first use by `by` at `label`. Calls with an
    * equal origin return the same conversation, including concurrent ones; it keeps the
    * principal that created it, and the label it was created at, whoever asks later.
    */
  def findOrCreate(origin: Origin, by: PrincipalId, label: Label)(using
      Tx^
  ): Either[StoreError, Conversation]

  /** The conversation for `origin`, if one was ever created; never creates one. */
  def find(origin: Origin)(using Tx^): Either[StoreError, Option[Conversation]]

  /** The conversation `id` names; `None` when there is none, never created or removed. */
  def get(id: ConversationId)(using Tx^): Either[StoreError, Option[Conversation]]

  /** The hosted call that made the post `conversation` begins with ([[Payload.Posted]]);
    * `None` when it begins otherwise, or that entry is gone.
    */
  def postedBy(conversation: ConversationId)(using Tx^): Either[StoreError, Option[CallSlot]]

  /** Deletes `conversation` with its entries, periods, verdicts and reviews, and its place and
    * its room when no other conversation is in either and no document was kept in the room;
    * nothing when it is gone already. Its usage,
    * profiles and documents are the caller's to delete.
    */
  def remove(conversation: ConversationId)(using Tx^): Either[StoreError, Unit]
}
