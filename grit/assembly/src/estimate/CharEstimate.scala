package grit.assembly.estimate

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.provider.TokenEstimator

/** The [[TokenEstimator]] for any model, by characters: a token per [[CharsPerToken]]
  * characters of what is sent, rounded up, plus [[PerMessage]] for the role framing of
  * each message and of the system prompt. It counts what the model is billed for: text,
  * tool calls and tool results. Reasoning counts nothing: a turn makes one model call, so
  * every reasoning block in a request is from an earlier turn, and a reasoning model drops
  * an earlier turn's reasoning before it bills (`grit.models.ReasoningBillingProbe`, 2026-09-24: gpt-oss-20b on two hosts billed
  * the same with and without it). A turn's own reasoning, resent within its tool loop, is
  * billed: revisit when the turn gets one.
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
          case AssistantBlock.Reasoning(_, _) => 0L
          case AssistantBlock.ToolCall(id, name, arguments) =>
            ToolCallId.value(id).length.toLong + name.length + ujson.write(arguments).length
        }.sum
    }
    PerMessage + ceiling(chars)
  }

  private def ceiling(chars: Long): Tokens = Tokens((chars + CharsPerToken - 1) / CharsPerToken)
}
