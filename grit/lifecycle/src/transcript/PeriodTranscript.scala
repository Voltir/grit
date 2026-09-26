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
  def of(entries: Vector[Entry]): String = entries.flatMap(e => line(e.payload)).mkString("\n\n")

  /** What the recorded windows among `entries` showed from other conversations
    * ([[Payload.Window]]'s nearby sections), read from `store` through `db`: each message
    * once, in the order first shown, as `[{place}] User: …` or `[{place}] Assistant: …`, as
    * [[of]] writes it. One gone since (its period closed and was purged) is left out.
    */
  def elsewhere(
      db: Db^,
      store: EntryStore,
      entries: Vector[Entry]
  ): Either[StoreError, Vector[String]] = {
    val shown = entries
      .flatMap(_.payload match {
        case Payload.Window(_, _, nearby) =>
          nearby.flatMap(n => n.entries.map(id => (n.place.written, id)))
        case _ => Vector.empty
      })
      .distinctBy(_._2)
    if (shown.isEmpty) Right(Vector.empty)
    else
      db.read(
        shown.foldLeft[Either[StoreError, Vector[String]]](Right(Vector.empty)) {
          case (acc, (place, id)) =>
            acc.flatMap(done =>
              store
                .get(id)
                .map(found => done ++ found.flatMap(e => line(e.payload)).map(l => s"[$place] $l"))
            )
        }
      )
  }

  private def line(payload: Payload): Option[String] = payload match {
    case Payload.Message(Message.User(text)) => Some(s"User: $text")
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      val said = blocks.collect { case AssistantBlock.Text(t) => t }.mkString.trim
      Option.when(said.nonEmpty)(s"Assistant: $said")
    case _ => None
  }
}
