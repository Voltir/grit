package grit.assembly.retrieval

import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.ModelRequest
import grit.core.store.{Entry, Payload}

/** What the query model is asked, and how its reply becomes a search query. A lexical
  * ranker finds nothing for a question that shares no words with its answer (ADR 0005), so
  * the query names what the earlier messages would contain rather than repeating the ask.
  */
object QueryWriter {

  val System: String =
    "You write search queries over the earlier part of a conversation between a user and " +
      "an assistant. Given the user's new message, write one query that would find the " +
      "earlier messages needed to answer it: the names, files, commands, " +
      "decisions and technical terms those messages would contain, with likely synonyms. " +
      "Write plain words: the search ranks by shared words, and ignores operators such as " +
      "AND, OR and quotes. Reply with the query alone, on one line."

  /** How much of a tool result the query model sees. */
  val ToolResultChars = 500

  /** The request for a query, from the turn's own entries, `own`. The earlier turns are
    * left out: shown the latest exchange, the model searched for its topic instead of the
    * new message's (the eval, 2026-09-24).
    */
  def request(own: Vector[Entry]): ModelRequest =
    ModelRequest(
      System,
      Vector(Message.User(own.flatMap(e => line(e.payload)).mkString("New message:\n", "\n", "")))
    )

  /** The query in `reply`: its text on one line, trimmed. Blank when it wrote nothing. */
  def text(reply: Message.Assistant): String =
    reply.blocks
      .collect { case AssistantBlock.Text(t) => t }
      .mkString(" ")
      .split("\\s+")
      .filter(_.nonEmpty)
      .mkString(" ")

  private def line(payload: Payload): Option[String] = payload match {
    case Payload.Message(Message.User(text)) => Some(s"User: $text")
    case Payload.Message(Message.Assistant(blocks, _, _, _, _)) =>
      val said = blocks.collect { case AssistantBlock.Text(t) => t }
      Option.when(said.nonEmpty)(s"Assistant: ${said.mkString("\n")}")
    case Payload.Message(Message.ToolResult(_, content, _)) =>
      Some(s"Tool result: ${content.take(ToolResultChars)}")
    // Assembly runs before the turn's tool loop, so its own entries hold no exchange yet.
    case Payload.Summary(_) | Payload.Query(_) | Payload.Window(_, _) | Payload.Topic(_) |
        Payload.Exchange(_) | Payload.Result(_, _) | Payload.Attempt(_) | Payload.Ask(_, _) =>
      None
  }
}
