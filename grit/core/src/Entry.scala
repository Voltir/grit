package grit.core

import java.time.Instant

/** One append-only row of conversation state.
  *
  * `turnSeq` is the turn the entry belongs to. `parentId` is the entry this one
  * was written in reply to — `None` for the root of a branch. `seq` orders
  * entries within the conversation and is assigned by the writer, not the
  * store.
  */
final case class Entry(
    id: EntryId,
    conversationId: ConversationId,
    turnSeq: TurnSeq,
    parentId: Option[EntryId],
    seq: Long,
    payload: Payload,
    createdAt: Instant
)
