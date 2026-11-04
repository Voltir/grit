package grit.assembly.estimate

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, Tokens}
import grit.core.provider.{TokenEstimator, ToolSchema}

/** A [[TokenEstimator]] by characters: a token per `charsPerToken` characters of text sent
  * (the system prompt, messages, tool calls and results) and per `schemaCharsPerToken` of
  * tool definitions, rounded up; [[PerMessage]] for each message's framing and the system
  * prompt's; `toolPreamble` for a request offering tools. [[CharEstimate$ CharEstimate]] is
  * the one grit runs; another is an estimator for another tokenizer, or a test's that must
  * not move when grit's is recalibrated.
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
  */
class CharEstimate(
    val charsPerToken: Double,
    val schemaCharsPerToken: Double,
    val toolPreamble: Tokens
) extends TokenEstimator {

  val PerMessage: Tokens = Tokens(4)

  def system(prompt: String): Tokens = PerMessage + text(prompt)

  /** `text` costed as sent with no framing: a token per `charsPerToken` characters, rounded
    * up.
    */
  def text(text: String): Tokens = ceiling(text.length.toLong, charsPerToken)

  override def tools(tools: Vector[ToolSchema]): Tokens =
    if (tools.isEmpty) Tokens.Zero
    else
      toolPreamble + ceiling(TokenEstimator.definitions(tools).length.toLong, schemaCharsPerToken)

  def message(message: Message): Tokens = {
    val byChars = PerMessage + ceiling(chars(message), charsPerToken)
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

  private def ceiling(chars: Long, perToken: Double): Tokens =
    Tokens(math.ceil(chars / perToken).toLong)
}

/** The estimator grit runs, erring high: 2.2 characters a token of text sent, 3.0 of tool
  * definitions, and a 300-token tool preamble. Measured against Anthropic's counts for
  * claude-haiku-5-5 and claude-opus-5-5 on coding turns (a system prompt and brief of
  * markdown dense with code names, tool definitions, and a transcript of code, compiler
  * output and JSON): every call of each turn estimated at 1.05 to 1.11 of the provider's
  * count. Plain English tokenizes lighter, so a conversation of prose reads higher than that;
  * a model with another tokenizer differs again, unmeasured.
  */
// Fitted on the two recorded turns: a call's input grew 1.4 to 1.8 times its growth at 4
// characters a token, the code-heavy transcript densest; at 2.2 the transcript's calls land at
// 1.05 to 1.07, and the first calls, heavier in tool definitions and prose, at 1.08 to 1.11.
// Solving the two first calls together (one tool set, different prose) put tool definitions
// near 3.1 characters a token, preamble included.
object CharEstimate extends CharEstimate(2.2, 3.0, Tokens(300))
