package grit.core.store

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnRef, TurnSeq}
import grit.core.place.Place
import grit.core.visibility.{Item, Label}

/** An in-memory [[EntryStore]] for tests, keeping [[StoreContract]] and [[ClearanceContract]]:
  * `conversation` is each conversation as its row holds it, whose label an entry copies when
  * it is inserted, and whose room it is read in. Its reads return only what the `Tx`'s
  * clearance reads; otherwise it ignores the `Tx`: writes are never rolled back, and `lockNext`
  * locks nothing. Unlike Postgres it takes any conversation id, known or not: an unknown one's
  * entries are public.
  */
final class InMemoryEntryStore(
    conversation: ConversationId -> Option[Conversation] = _ => None
) extends EntryStore {

  @caps.unsafe.untrackedCaptures
  private var entries = Vector.empty[Entry]

  /** Each entry's label, copied from its conversation as it was inserted. */
  @caps.unsafe.untrackedCaptures
  private var labels = Map.empty[EntryId, Label]

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
      labels =
        labels.updated(entry.id, conversation(entry.conversationId).fold(Label.Public)(_.label))
      val (turn, seq) = marks.getOrElse(entry.conversationId, (TurnSeq.First, EntrySeq.First))
      marks = marks.updated(
        entry.conversationId,
        (
          if (entry.turnSeq >= turn) entry.turnSeq.next else turn,
          if (entry.seq >= seq) entry.seq.next else seq
        )
      )
      Right(())
    }

  /** Deletes every entry `doomed` picks: the in-memory form of a purge's `DELETE`. */
  private[store] def remove(doomed: Entry -> Boolean): Unit =
    entries = entries.filterNot(doomed)

  /** `conversation`'s entries deleted and its next positions forgotten, as Postgres's cascade
    * from a removed conversation deletes them with its row.
    */
  private[store] def forget(conversation: ConversationId): Unit = {
    entries = entries.filterNot(_.conversationId == conversation)
    marks = marks - conversation
  }

  /** Every entry kept, in the order written, whatever its label: what a fake writing or
    * counting across conversations scans.
    */
  def everything: Vector[Entry] = entries

  /** Every entry kept that `tx`'s clearance reads, in the order written: what a fake reading
    * content across conversations scans.
    */
  def readable(using tx: Tx^): Vector[Entry] =
    entries.filter(e => readsIn(e.conversationId, labels.getOrElse(e.id, Label.Public)))

  /** Whether `tx`'s clearance reads what `c` holds: its entries, and where it happens. */
  def reads(c: ConversationId)(using tx: Tx^): Boolean =
    readsIn(c, conversation(c).fold(Label.Public)(_.label))

  /** Whether `tx` reads `label` recorded in `c`'s room, as the SQL store's filter decides. */
  private def readsIn(c: ConversationId, label: Label)(using tx: Tx^): Boolean =
    Tx.clearance(tx)
      .reads(Item.InRoom(conversation(c).fold(Place.Everywhere)(_.origin.room)), label)

  def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] =
    Right(readable.find(_.id == id))

  def list(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] =
    Right(readable.filter(_.conversationId == conversation).sortBy(_.seq))

  def at(conversation: ConversationId, seqs: Vector[EntrySeq])(using
      Tx^
  ): Either[StoreError, Vector[Entry]] =
    list(conversation).map(_.filter(e => seqs.contains(e.seq)))

  def ofTurn(turn: TurnRef)(using Tx^): Either[StoreError, Vector[Entry]] =
    list(turn.conversationId).map(_.filter(_.turnSeq == turn.turnSeq))

  def lockNext(conversation: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] = {
    val (turn, seq) = marks.getOrElse(conversation, (TurnSeq.First, EntrySeq.First))
    Right(EntryStore.Next(turn, seq))
  }
}
