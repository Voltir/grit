package grit.core

import java.time.Instant

/** A conversation: the entries and turns that share one [[Origin]]. */
final case class Conversation(
    id: ConversationId,
    origin: Origin,
    createdAt: Instant
)
