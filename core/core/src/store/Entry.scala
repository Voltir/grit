package grit.core.store

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq}
import grit.core.visibility.Integrity

/** One append-only row of conversation state.
  *
  * `turnSeq` is the turn the entry belongs to. `parentId` is the entry this one
  * was written in reply to — `None` for the root of a branch. `integrity` is how far it may be
  * believed, fixed when it was written (ADR 0036): an inbound one's is its author's met with its
  * room's cap, applied by [[EntryStore.insert]].
  */
final case class Entry(
    id: EntryId,
    conversationId: ConversationId,
    turnSeq: TurnSeq,
    parentId: Option[EntryId],
    seq: EntrySeq,
    payload: Payload,
    createdAt: Instant,
    integrity: Integrity
)
