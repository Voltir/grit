package grit.assembly.estimate

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.provider.TokenEstimator

/** The [[TokenEstimator]] for any model, by characters: a token per [[CharsPerToken]]
  * characters of what is sent, rounded up, plus [[PerMessage]] for the role framing of
  * each message and of the system prompt. It counts what the model is billed for: text,
  * tool calls and tool results. A reasoning block a provider replays (its `replay` kept) is
  * sent back and counts its text: a turn's own, within its tool loop, and a carried turn's
  * (ADR 0052). One with nothing to replay is not sent back and counts nothing.
  * Uncalibrated; English prose runs close to it, code and JSON above it.
  */
object CharEstimate extends TokenEstimator {

  val CharsPerToken = 4

  val PerMessage: Tokens = Tokens(4)

  def system(prompt: String): Tokens = PerMessage + ceiling(prompt.length.toLong)

  def message(message: Message): Tokens = {
    val chars = message match {
      case Message.User(text) => text.length.toLong
      case Message.ToolResult(callId, content, _) =>
        ToolCallId.value(callId).length.toLong + content.length
      case Message.Assistant(blocks, _, _, _, _) =>
        blocks.map {
          case AssistantBlock.Text(text) => text.length.toLong
          case AssistantBlock.Reasoning(text, Some(_)) => text.length.toLong
          case AssistantBlock.Reasoning(_, None) => 0L
          case AssistantBlock.ToolCall(id, name, arguments) =>
            ToolCallId.value(id).length.toLong + name.length + ujson.write(arguments).length
        }.sum
    }
    PerMessage + ceiling(chars)
  }

  private def ceiling(chars: Long): Tokens = Tokens((chars + CharsPerToken - 1) / CharsPerToken)
}
