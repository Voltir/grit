package grit.assembly

import grit.core.{AssistantBlock, Message, Tokens, ToolCallId}

/** A rough, provider-independent count of the tokens a message costs in a request: a
  * token per [[CharsPerToken]] characters of what is sent, rounded up, plus
  * [[PerMessage]] for the role framing. It counts what goes over the wire: text, tool
  * calls, tool results, and reasoning's replay form (its readable text is not sent).
  * Uncalibrated; English prose runs close to it, code and JSON above it.
  */
object TokenEstimate {

  val CharsPerToken = 4

  val PerMessage: Tokens = Tokens(4)

  /** The estimated cost of sending `message`. */
  def of(message: Message): Tokens = {
    val chars = message match {
      case Message.User(text) => text.length.toLong
      case Message.ToolResult(callId, content, _) =>
        ToolCallId.value(callId).length.toLong + content.length
      case Message.Assistant(blocks, _, _, _) =>
        blocks.map {
          case AssistantBlock.Text(text) => text.length.toLong
          case AssistantBlock.Reasoning(_, replay) =>
            replay.fold(0L)(r => ujson.write(r).length.toLong)
          case AssistantBlock.ToolCall(id, name, arguments) =>
            ToolCallId.value(id).length.toLong + name.length + ujson.write(arguments).length
        }.sum
    }
    PerMessage + Tokens((chars + CharsPerToken - 1) / CharsPerToken)
  }
}
