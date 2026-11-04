package grit.assembly.estimate

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.provider.{TokenEstimator, ToolSchema}

/** The [[TokenEstimator]] for any model, by characters: a token per [[CharsPerToken]]
  * characters of what is sent, rounded up, plus [[PerMessage]] for the role framing of
  * each message and of the system prompt. It counts what the model is billed for: text,
  * tool calls and tool results.
  *
  * An assistant message costs at least its own output count
  * ([[grit.core.message.Usage.output]]): sent back, what a model wrote is billed about as it
  * was counted, and replayed reasoning (a block whose `replay` is kept) in full, though its
  * text is at most a summary of it. The exception is a message holding reasoning with nothing
  * to replay: that is not sent back, its output count includes it, and the message costs its
  * characters. The estimate is high by the difference for a message grit rewrote before
  * sending (a call dropped), for a model that reasons without returning its reasoning (its
  * output count holds it), and for a carried turn's (ADR 0052) replayed reasoning on a model
  * that drops the reasoning of turns before the last user message.
  * Uncalibrated; English prose runs close to it, code and JSON above it.
  */
object CharEstimate extends TokenEstimator {

  val CharsPerToken = 4

  val PerMessage: Tokens = Tokens(4)

  val SchemaCharsPerToken: Double = 3.0

  /** What a provider adds to a request offering tools: its tool-use preamble. */
  val ToolPreamble: Tokens = Tokens(300)

  def system(prompt: String): Tokens = PerMessage + text(prompt)

  /** `text` costed as sent with no framing: a token per [[CharsPerToken]] characters, rounded
    * up.
    */
  def text(text: String): Tokens = ceiling(text.length.toLong)

  override def tools(tools: Vector[ToolSchema]): Tokens =
    if (tools.isEmpty) Tokens.Zero
    else
      ToolPreamble +
        Tokens(math.ceil(TokenEstimator.definitions(tools).length / SchemaCharsPerToken).toLong)

  def message(message: Message): Tokens = {
    val byChars = PerMessage + ceiling(chars(message))
    message match {
      case Message.Assistant(blocks, _, usage, _, _) if !blocks.exists(withheld) =>
        val counted = PerMessage + usage.output
        if (Tokens.value(counted) > Tokens.value(byChars)) counted else byChars
      case _ => byChars
    }
  }

  private def withheld(block: AssistantBlock): Boolean = block match {
    case AssistantBlock.Reasoning(_, None) => true
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
