package grit.core.store

import java.time.Instant

import grit.core.id.ConversationId

/** A conversation: the entries and turns that share one [[Origin]]. */
final case class Conversation(
    id: ConversationId,
    origin: Origin,
    createdAt: Instant
)
