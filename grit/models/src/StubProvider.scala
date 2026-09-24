package grit.models

import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, Provider, ProviderError}

/** A [[Provider]] that calls no model: it answers every request by quoting its last
  * message, at no cost. For running a turn end to end without spending anything.
  */
final class StubProvider extends Provider {

  def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
    val last = request.messages.lastOption match {
      case Some(Message.User(text)) => text
      case Some(Message.ToolResult(_, content, _)) => content
      case Some(Message.Assistant(_, _, _, _)) => "(an assistant message)"
      case None => "(nothing)"
    }
    Right(
      Message.Assistant(
        Vector(AssistantBlock.Text(s"stub reply to: $last")),
        StopReason.EndTurn,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0))),
        StubProvider.Model
      )
    )
  }
}

object StubProvider {

  /** The model id a stub reply records. */
  val Model = "grit/stub"
}
