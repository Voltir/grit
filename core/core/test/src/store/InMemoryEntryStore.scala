package grit.core.store

import scala.math.Ordering.Implicits.infixOrderingOps

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnSeq}

/** An in-memory [[EntryStore]] for tests, keeping [[StoreContract]]. It ignores the `Tx`:
  * writes are never rolled back, and `lockNext` locks nothing. Unlike Postgres it takes any
  * conversation id, known or not.
  */
final class InMemoryEntryStore extends EntryStore {

  @caps.unsafe.untrackedCaptures
  private var entries = Vector.empty[Entry]

  /** Each conversation's next turn and entry seq, past every entry ever inserted, as
    * `grit.conversations` keeps them: a purge does not lower them.
    */
  @caps.unsafe.untrackedCaptures
  private var marks = Map.empty[ConversationId, (TurnSeq, EntrySeq)]

  def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] =
    if (entries.exists(_.id == entry.id)) {
      Left(StoreError.DuplicateId(entry.id))
    } else if (
      entries.exists(e => e.conversationId == entry.conversationId && e.seq == entry.seq)
    ) {
      Left(StoreError.DatabaseError(s"seq ${entry.seq} is taken in its conversation"))
    } else {
      entries = entries :+ entry
      val (turn, seq) = marks.getOrElse(entry.conversationId, (TurnSeq.First, EntrySeq.First))
      marks = marks.updated(
        entry.conversationId,
        (
          if (TurnSeq.value(entry.turnSeq) >= TurnSeq.value(turn)) entry.turnSeq.next else turn,
          if (entry.seq >= seq) entry.seq.next else seq
        )
      )
      Right(())
    }

  /** Deletes every entry `doomed` picks: the in-memory form of a purge's `DELETE`. */
  private[store] def remove(doomed: Entry -> Boolean): Unit =
    entries = entries.filterNot(doomed)

  /** Every entry kept, in the order written: what a fake reading across conversations scans. */
  def everything: Vector[Entry] = entries

  def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] =
    Right(entries.find(_.id == id))

  def list(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] =
    Right(entries.filter(_.conversationId == conversation).sortBy(_.seq))

  def lockNext(conversation: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] = {
    val (turn, seq) = marks.getOrElse(conversation, (TurnSeq.First, EntrySeq.First))
    Right(EntryStore.Next(turn, seq))
  }
}
