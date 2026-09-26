package grit.lifecycle.transcript

import grit.core.id.{PeriodRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message}
import grit.core.store.{Db, Entry, EntryStore, Payload, StoreError}

/** A period as a classifier or the summary model reads it. */
object PeriodTranscript {

  /** The entries of `period`'s turns `first` to `last`, oldest first, read from `store`
    * through `db`.
    */
  def entries(
      db: Db^,
      store: EntryStore,
      period: PeriodRef,
      first: TurnSeq,
      last: TurnSeq
  ): Either[StoreError, Vector[Entry]] =
    db.read(store.list(period.conversationId))
      .map(_.filter { e =>
        val t = TurnSeq.value(e.turnSeq)
        t >= TurnSeq.value(first) && t <= TurnSeq.value(last)
      })

  /** `entries` as one transcript: each user message as `User: …` and each reply's text as
    * `Assistant: …`, in order, a blank line between them; a reply with no text, and every
    * other kind of entry, left out.
    */
  def of(entries: Vector[Entry]): String =
    entries
      .flatMap(_.payload match {
        case Payload.Message(Message.User(text)) => Some(s"User: $text")
        case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
          val said = blocks.collect { case AssistantBlock.Text(t) => t }.mkString.trim
          Option.when(said.nonEmpty)(s"Assistant: $said")
        case _ => None
      })
      .mkString("\n\n")
}
