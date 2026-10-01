package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, PrincipalId}

/** A conversation: the entries and turns that share one [[Origin]], and who began it. */
final case class Conversation(
    id: ConversationId,
    origin: Origin,
    createdBy: PrincipalId,
    createdAt: Instant
)
