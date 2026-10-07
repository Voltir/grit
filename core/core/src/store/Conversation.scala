package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, PrincipalId}
import grit.core.visibility.Label

/** A conversation: the entries and turns that share one [[Origin]], who began it, and the
  * label it was created at, which every entry recorded in it is kept at (ADR 0030).
  */
final case class Conversation(
    id: ConversationId,
    origin: Origin,
    createdBy: PrincipalId,
    createdAt: Instant,
    label: Label
)
