package grit.assembly.estimate

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.provider.{TokenEstimator, ToolSchema}

/** The [[TokenEstimator]] for any model, by characters: a token per [[CharsPerToken]]
  * characters of what is sent, rounded up, plus [[PerMessage]] for the role framing of
  * each message and of the system prompt. It counts what the model is billed for: text,
  * tool calls and tool results.
  *
  * A message that replays reasoning (a block whose `replay` is kept) costs at least its own
  * output count ([[grit.core.message.Usage.output]]): the provider bills the replayed
  * reasoning in full, and a block's text is at most a summary of it. That holds within a
  * turn's tool loop; on a model that drops the reasoning of turns before the last user
  * message, a carried turn's (ADR 0052) is costed but not billed, and the estimate is high by
  * it. Reasoning with nothing to replay is not sent back and costs nothing.
  * Uncalibrated; English prose runs close to it, code and JSON above it.
  */
object CharEstimate extends TokenEstimator {

  val CharsPerToken = 4

  val PerMessage: Tokens = Tokens(4)

  val SchemaCharsPerToken: Double = 3.0

  /** What a provider adds to a request offering tools: its tool-use preamble. */
  val ToolPreamble: Tokens = Tokens(300)

  def system(prompt: String): Tokens = PerMessage + ceiling(prompt.length.toLong)

  override def tools(tools: Vector[ToolSchema]): Tokens =
    if (tools.isEmpty) Tokens.Zero
    else
      ToolPreamble +
        Tokens(math.ceil(TokenEstimator.definitions(tools).length / SchemaCharsPerToken).toLong)

  def message(message: Message): Tokens = {
    val byChars = PerMessage + ceiling(chars(message))
    message match {
      case Message.Assistant(blocks, _, usage, _, _) if blocks.exists(replays) =>
        val counted = PerMessage + usage.output
        if (Tokens.value(counted) > Tokens.value(byChars)) counted else byChars
      case _ => byChars
    }
  }

  private def replays(block: AssistantBlock): Boolean = block match {
    case AssistantBlock.Reasoning(_, Some(_)) => true
    case _ => false
  }

  private def chars(message: Message): Long = message match {
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

  private def ceiling(chars: Long): Tokens = Tokens((chars + CharsPerToken - 1) / CharsPerToken)
}
