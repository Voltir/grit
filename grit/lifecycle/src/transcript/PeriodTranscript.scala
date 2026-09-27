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

  /** The most characters a tool line holds, its label and an ending "…" included. */
  val ToolChars = 120

  /** `entries` as [[Labelled]]: each user message and reply text as [[of]] renders it, and
    * each tool result as its call as shown, " → ", and its content on one line, "failed: "
    * first when it failed, clipped to [[ToolChars]]. Unless `questionsGround`, a user
    * message ending in "?" (trimmed) is a question, which grounds nothing (an eval toggle:
    * kept as the rule, or removed, once measured).
    */
  def labelled(entries: Vector[Entry], questionsGround: Boolean = true): Labelled = {
    // Who wrote each line, and its text after the label.
    val written: Vector[(Labelled.Source, String)] = entries.flatMap(e =>
      e.payload match {
        case Payload.Result(result, shown) =>
          val content = result.content.trim.split("\\s+").mkString(" ")
          val failed = if (result.isError) "failed: " else ""
          Some(Labelled.Source.Tool(!result.isError) -> s"$shown → $failed$content")
        case Payload.Message(Message.User(text)) =>
          // Under the question rule, a question is not the person establishing anything.
          val source =
            if (!questionsGround && text.trim.endsWith("?")) Labelled.Source.Asked
            else Labelled.Source.Person
          line(e.payload).map(source -> _)
        case other => line(other).map(Labelled.Source.Assistant -> _)
      }
    )
    Labelled.of(written.zipWithIndex.map { case ((source, text), i) =>
      val letter = source match {
        case Labelled.Source.Person | Labelled.Source.Asked => "u"
        case Labelled.Source.Assistant => "a"
        case Labelled.Source.Tool(_) => "t"
      }
      val label = s"$letter${i + 1}"
      val rendered = s"[$label] $text"
      val clipped = source match {
        case Labelled.Source.Tool(_) if rendered.length > ToolChars =>
          rendered.take(ToolChars - 1) + "…"
        case _ => rendered
      }
      Labelled.Line(label, source, clipped)
    })
  }

  private def line(payload: Payload): Option[String] = payload match {
    case Payload.Message(Message.User(text)) => Some(s"User: $text")
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      val said = blocks.collect { case AssistantBlock.Text(t) => t }.mkString.trim
      Option.when(said.nonEmpty)(s"Assistant: $said")
    case _ => None
  }
}
