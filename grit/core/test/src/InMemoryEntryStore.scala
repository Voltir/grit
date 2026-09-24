package grit.core

/** An in-memory [[EntryStore]] for tests. It ignores the `Tx`: writes are never rolled
  * back, and `lockNext` locks nothing.
  */
final class InMemoryEntryStore extends EntryStore {

  @caps.unsafe.untrackedCaptures
  private var entries = Vector.empty[Entry]

  def insert(entry: Entry)(using Tx^): Either[StoreError, Unit] =
    if (entries.exists(_.id == entry.id)) {
      Left(StoreError.DuplicateId(entry.id))
    } else {
      entries = entries :+ entry
      Right(())
    }

  def get(id: EntryId)(using Tx^): Either[StoreError, Option[Entry]] =
    Right(entries.find(_.id == id))

  def list(conversation: ConversationId)(using Tx^): Either[StoreError, Vector[Entry]] =
    Right(entries.filter(_.conversationId == conversation).sortBy(_.seq))

  def lockNext(conversation: ConversationId)(using Tx^): Either[StoreError, EntryStore.Next] = {
    val mine = entries.filter(_.conversationId == conversation)
    Right(
      EntryStore.Next(
        mine.map(_.turnSeq).maxByOption(TurnSeq.value).fold(TurnSeq.First)(_.next),
        mine.map(_.seq).maxOption.fold(0L)(_ + 1)
      )
    )
  }
}
