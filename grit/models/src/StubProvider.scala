package grit.models

import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{Delta, ModelRequest, Provider, ProviderError}

/** A [[Provider]] that calls no model: it answers every request by quoting its last
  * message, at no cost, after `delayMs` (a slow model, for watching a turn run). For
  * running a turn end to end without spending anything.
  */
final class StubProvider(delayMs: Long = 0) extends Provider {

  def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
    if (delayMs > 0) Thread.sleep(delayMs)
    Right(reply(request))
  }

  /** The reply word by word, the delay spread across the words. */
  override def stream(
      request: ModelRequest,
      onDelta: Delta => Unit
  ): Either[ProviderError, Message.Assistant] = {
    val answer = reply(request)
    val words = StubProvider.words(answer.blocks.collect { case AssistantBlock.Text(t) =>
      t
    }.mkString)
    words.foreach { word =>
      if (delayMs > 0) Thread.sleep(delayMs / math.max(1, words.size))
      onDelta(Delta.Text(word))
    }
    Right(answer)
  }

  private def reply(request: ModelRequest): Message.Assistant = {
    val last = request.messages.lastOption match {
      case Some(Message.User(text)) => text
      case Some(Message.ToolResult(_, content, _)) => content
      case Some(Message.Assistant(_, _, _, _)) => "(an assistant message)"
      case None => "(nothing)"
    }
    Message.Assistant(
      Vector(AssistantBlock.Text(s"stub reply to: $last")),
      StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0))),
      StubProvider.Model
    )
  }
}

object StubProvider {

  /** The model id a stub reply records. */
  val Model = "grit/stub"

  /** `text` cut before each space, so the pieces join back to it exactly. */
  def words(text: String): Vector[String] =
    text.split("(?= )").toVector.filter(_.nonEmpty)
}
