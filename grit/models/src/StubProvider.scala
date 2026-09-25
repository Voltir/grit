package grit.models

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{Delta, ModelRequest, Provider, ProviderError, ToolUse}

/** A [[Provider]] that calls no model: it answers every request by quoting the last user
  * message, at no cost, after `delayMs` (a slow model, for watching a turn run). For
  * running a turn end to end without spending anything.
  *
  * Offered tools it may call ([[ToolUse.Auto]]) while the last message is the user's, it
  * calls the first, as a model can, with a line of text beside the call: the arguments
  * are the JSON after `#call:` in that message (to the end of its line), or `{}`. Its id is
  * [[StubProvider.CallId]].
  */
final class StubProvider(delayMs: Long = 0) extends Provider {

  def complete(request: ModelRequest): Either[ProviderError, Message.Assistant] = {
    if (delayMs > 0) Thread.sleep(delayMs)
    Right(reply(request))
  }

  /** The reply's text word by word, the delay spread across the words, then its call. */
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
    answer.blocks.foreach {
      case AssistantBlock.ToolCall(_, name, _) => onDelta(Delta.Calling(name))
      case _ => ()
    }
    Right(answer)
  }

  private def reply(request: ModelRequest): Message.Assistant = {
    val asked = request.messages.reverseIterator.collectFirst { case Message.User(text) => text }
    val call = (request.use, request.tools.headOption, request.messages.lastOption) match {
      case (ToolUse.Auto, Some(tool), Some(Message.User(text))) =>
        Some(tool.name -> StubProvider.arguments(text))
      case _ => None
    }
    val said = call match {
      case Some((name, _)) => s"stub calls $name"
      case None =>
        request.messages.lastOption match {
          case Some(Message.User(text)) => s"stub reply to: $text"
          case Some(Message.ToolResult(_, content, _)) =>
            s"stub reply to: ${asked.getOrElse(content)}"
          case Some(Message.Assistant(_, _, _, _)) => "stub reply to: (an assistant message)"
          case None => "stub reply to: (nothing)"
        }
    }
    Message.Assistant(
      Vector(AssistantBlock.Text(said)) ++ call.toVector.map((name, args) =>
        AssistantBlock.ToolCall(StubProvider.CallId, name, args)
      ),
      if (call.nonEmpty) StopReason.ToolUse else StopReason.EndTurn,
      Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, Some(BigDecimal(0))),
      StubProvider.Model
    )
  }
}

object StubProvider {

  /** The model id a stub reply records. */
  val Model = "grit/stub"

  /** The id of every tool call the stub makes. */
  val CallId: ToolCallId = ToolCallId("stub-call")

  /** The marker before a tool call's arguments in a user message. */
  val CallMarker = "#call:"

  /** `text` cut before each space, so the pieces join back to it exactly. */
  def words(text: String): Vector[String] =
    text.split("(?= )").toVector.filter(_.nonEmpty)

  /** The JSON after [[CallMarker]] in `text`, to the end of its line; `{}` without a marker,
    * and the raw string when it is not JSON (as a model can send).
    */
  def arguments(text: String): ujson.Value = {
    val at = text.indexOf(CallMarker)
    if (at < 0) ujson.Obj()
    else {
      val raw = text.drop(at + CallMarker.length).takeWhile(_ != '\n').trim
      scala.util.Try(ujson.read(raw)).getOrElse(ujson.Str(raw))
    }
  }
}
