package grit

import java.time.Instant

/** One append-only row of conversation state.
  *
  * `parentId` is the entry this one was written in reply to — `None` for the
  * root of a branch. `seq` orders entries within a branch and is assigned by
  * the writer, not the store.
  */
final case class Entry(
    id: EntryId,
    parentId: Option[EntryId],
    seq: Long,
    payload: ujson.Value,
    createdAt: Instant
)
