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
      "an assistant. Given the latest exchange and the user's new message, write one query " +
      "that would find the earlier messages needed to answer it: the names, files, commands, " +
      "decisions and technical terms those messages would contain, with likely synonyms. " +
      "Reply with the query alone, on one line. If the latest exchange already holds " +
      "everything needed, reply NONE."

  /** The word a reply uses to say no search is needed. */
  val NoSearch: String = "NONE"

  /** How much of a tool result the query model sees. */
  val ToolResultChars = 500

  /** The request for a query, from the latest earlier turns' entries, `latest`, and the
    * turn's own, `own`.
    */
  def request(latest: Vector[Entry], own: Vector[Entry]): ModelRequest = {
    val before = latest.flatMap(e => line(e.payload))
    val now = own.flatMap(e => line(e.payload))
    val text =
      (if (before.isEmpty) "" else before.mkString("Latest exchange:\n", "\n", "\n\n")) +
        now.mkString("New message:\n", "\n", "")
    ModelRequest(System, Vector(Message.User(text)))
  }

  /** The query in `reply`: its text on one line, trimmed. Blank when it wrote nothing. */
  def text(reply: Message.Assistant): String =
    reply.blocks
      .collect { case AssistantBlock.Text(t) => t }
      .mkString(" ")
      .split("\\s+")
      .filter(_.nonEmpty)
      .mkString(" ")

  /** Whether `query` asks for a search: not blank, and not [[NoSearch]]. */
  def searches(query: String): Boolean =
    query.nonEmpty && query.stripSuffix(".").toUpperCase != NoSearch

  private def line(payload: Payload): Option[String] = payload match {
    case Payload.Message(Message.User(text)) => Some(s"User: $text")
    case Payload.Message(Message.Assistant(blocks, _, _, _)) =>
      val said = blocks.collect { case AssistantBlock.Text(t) => t }
      Option.when(said.nonEmpty)(s"Assistant: ${said.mkString("\n")}")
    case Payload.Message(Message.ToolResult(_, content, _)) =>
      Some(s"Tool result: ${content.take(ToolResultChars)}")
    case Payload.Summary(_) => None
  }
}
