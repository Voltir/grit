package grit.core.context

import grit.core.message.{AssistantBlock, Message}
import grit.core.place.Place
import grit.core.store.{Entry, Payload}

/** What the model is shown of an entry a window names: the one definition, which an
  * assembler costs and the turn sends.
  */
object Shown {

  /** A message as it is; a closing entry as one user message, [[grit.core.period.Closing.shown]]
    * dated by the entry; `None` for any other entry.
    */
  def of(entry: Entry): Option[Message] = entry.payload match {
    case Payload.Message(m) => Some(m)
    case Payload.Closed(_, reason, closing) =>
      Some(Message.User(closing.shown(entry.createdAt, reason)))
    case _ => None
  }

  /** A nearby section as the model is shown it: one user message, "From another
    * conversation of yours, still open, at {place.written}:", then each message among
    * `entries` as a `User:` or `Assistant:` line of its text. `None` when none of them has
    * text.
    */
  def nearby(place: Place, entries: Vector[Entry]): Option[Message] = {
    val lines = entries.flatMap(e => line(e.payload))
    Option.when(lines.nonEmpty)(
      Message.User(
        (s"From another conversation of yours, still open, at ${place.written}:" +: lines)
          .mkString("\n")
      )
    )
  }

  private def line(payload: Payload): Option[String] = payload match {
    case Payload.Message(Message.User(text)) => Option.when(text.trim.nonEmpty)(s"User: $text")
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      val said = blocks.collect { case AssistantBlock.Text(t) => t }.mkString("\n")
      Option.when(said.trim.nonEmpty)(s"Assistant: $said")
    case _ => None
  }
}
