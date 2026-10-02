package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq}

/** One append-only row of conversation state.
  *
  * `turnSeq` is the turn the entry belongs to. `parentId` is the entry this one
  * was written in reply to — `None` for the root of a branch.
  */
final case class Entry(
    id: EntryId,
    conversationId: ConversationId,
    turnSeq: TurnSeq,
    parentId: Option[EntryId],
    seq: EntrySeq,
    payload: Payload,
    createdAt: Instant
)
