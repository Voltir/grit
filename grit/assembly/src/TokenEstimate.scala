package grit.assembly

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, Tokens}

/** A rough, provider-independent count of the tokens a message costs in a request: a
  * token per [[CharsPerToken]] characters of what is sent, rounded up, plus
  * [[PerMessage]] for the role framing. It counts what the model is billed for: text, tool
  * calls and tool results. Reasoning counts nothing: every message it is asked about is
  * from an earlier turn, and a reasoning model drops an earlier turn's reasoning before it
  * bills (`grit.models.ReasoningBillingProbe`, 2026-09-24: gpt-oss-20b on two hosts billed
  * the same with and without it). A turn's own reasoning, resent within its tool loop, is
  * billed: revisit when the turn gets one.
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
          case AssistantBlock.Reasoning(_, _) => 0L
          case AssistantBlock.ToolCall(id, name, arguments) =>
            ToolCallId.value(id).length.toLong + name.length + ujson.write(arguments).length
        }.sum
    }
    PerMessage + Tokens((chars + CharsPerToken - 1) / CharsPerToken)
  }
}
