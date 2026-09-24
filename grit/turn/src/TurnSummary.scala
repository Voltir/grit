package grit.turn

import grit.core.id.{EntryId, ToolCallId, TurnRef, WorkflowId}
import grit.core.message.{AssistantBlock, Message}
import grit.core.provider.ModelRequest
import grit.core.store.{Entry, Payload}

/** A turn's short summary: what the summary model is asked, and what of its reply is kept.
  * The summary is written from the turn's own messages alone, so it reads on its own
  * wherever it is retrieved.
  */
object TurnSummary {

  val System: String =
    "You write the memory of a conversation. Summarise the exchange below in one or two " +
      "sentences: what was asked, and what was answered or decided. Keep names, numbers, " +
      "file names and identifiers exactly as written. Reply with the summary only."

  /** How much of a tool result the summary model sees. The store keeps all of it. */
  val ToolResultChars = 2000

  /** The id of `turn`'s summary entry. */
  def id(turn: TurnRef): EntryId =
    EntryId(s"summary:${WorkflowId.value(turn.workflowId)}")

  /** The request that summarises the turn whose entries are `own`, as one transcript. */
  def request(own: Vector[Entry]): ModelRequest =
    ModelRequest(
      System,
      Vector(Message.User(own.flatMap(e => line(e.payload)).mkString("\n\n")))
    )

  /** The summary in `reply`: its text, trimmed; `None` when it has none. */
  def text(reply: Message.Assistant): Option[String] =
    Some(reply.blocks.collect { case AssistantBlock.Text(t) => t }.mkString.trim)
      .filter(_.nonEmpty)

  private def line(payload: Payload): Option[String] = payload match {
    case Payload.Message(Message.User(text)) => Some(s"User: $text")
    case Payload.Message(Message.Assistant(blocks, _, _, _)) =>
      val said = blocks.collect {
        case AssistantBlock.Text(t) => t
        case AssistantBlock.ToolCall(id, name, arguments) =>
          s"[called $name (${ToolCallId.value(id)}) with ${arguments.render()}]"
      }
      Option.when(said.nonEmpty)(s"Assistant: ${said.mkString("\n")}")
    case Payload.Message(Message.ToolResult(id, content, isError)) =>
      val kind = if (isError) "Tool error" else "Tool result"
      Some(s"$kind (${ToolCallId.value(id)}): ${content.take(ToolResultChars)}")
    case Payload.Summary(_) | Payload.Query(_) => None
  }
}
