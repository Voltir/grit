package grit.core

/** Append-only entry store. Entries are never rewritten or deleted. */
trait EntryStore {

  /** Appends `entry`, rejecting an existing id with `DuplicateId`.
    * Never overwrites.
    */
  def insert(entry: Entry)(using Tx^): Either[StoreError, Unit]

  /** The entry with `id`, or `None` when no such entry exists. */
  def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]]

  /** Every entry in `conversation`, ascending by `seq`. Empty for an unknown conversation. */
  def list(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]]

  /** Locks `conversation` against other writers until the transaction ends, and returns
    * the positions after everything already recorded in it. Every writer of a
    * conversation's entries takes this lock first, then writes at the positions returned.
    */
  def lockNext(conversation: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next]
}

object EntryStore {

  /** The next unused turn seq and entry seq of a conversation. */
  final case class Next(turnSeq: TurnSeq, seq: Long)
}
