package grit.core

/** Identifier for a [[Conversation]]. Assigned by the store when the
  * conversation is created; never contains `:`, so `{conversationId}:{turnSeq}`
  * is an unambiguous workflow id.
  */
opaque type ConversationId = String

object ConversationId {
  def apply(value: String): ConversationId = value
  def value(id: ConversationId): String = id
}
