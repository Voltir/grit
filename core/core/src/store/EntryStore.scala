package grit.core.store

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnRef, TurnSeq}

/** Append-only entry store. Entries are never rewritten, and deleted only by the purge of
  * their closed period ([[PeriodStore.purge]]). Its reads return only the entries their
  * transaction's clearance reads ([[Tx.clearance]], an entry read as
  * [[grit.core.visibility.Item.InRoom]] of its conversation's room): one it does not read is
  * left out as if it were not kept.
  */
trait EntryStore {

  /** Appends `entry`, kept at the label its conversation was created at
    * ([[Conversation.label]]). Never overwrites: an id already taken is `DuplicateId`, and a
    * `seq` already taken in its conversation is a `DatabaseError` (positions come from
    * [[lockNext]]).
    */
  def insert(entry: Entry)(using Tx^): Either[StoreError, Unit]

  /** The entry with `id`, or `None` when no such entry exists or its transaction does not
    * read it.
    */
  def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]]

  /** Every entry in `conversation` its transaction reads, ascending by `seq`. Empty for an
    * unknown conversation.
    */
  def list(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]]

  /** The entries of `conversation` at `seqs`, ascending by seq, each once however often its
    * seq is given. A seq with no entry (never written, or purged with its period), or one its
    * transaction does not read, is left out, so the result can be shorter than `seqs`.
    */
  def at(conversation: ConversationId, seqs: Vector[EntrySeq])(using
      Tx^
  ): Either[StoreError, Vector[Entry]]

  /** `turn`'s entries its transaction reads, ascending by seq; empty when it has none. */
  def ofTurn(turn: TurnRef)(using Tx^): Either[StoreError, Vector[Entry]]

  /** Locks `conversation` against other writers until the transaction ends, and returns the
    * positions after every entry ever recorded in it, purged ones included. Every writer of a
    * conversation's entries takes this lock first, then writes at the positions returned.
    */
  def lockNext(conversation: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next]
}

object EntryStore {

  /** The next unused turn seq and entry seq of a conversation. */
  final case class Next(turnSeq: TurnSeq, seq: EntrySeq)
}
