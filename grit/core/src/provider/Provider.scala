package grit.core.provider

import grit.core.message.{AssistantBlock, Message}

/** Capability to call a model. Each call is one request with no memory of earlier ones:
  * everything the model sees is in the request.
  */
trait Provider extends caps.SharedCapability {

  /** One model response to `request`. Called once; a caller that wants a retry makes it. */
  def complete(request: ModelRequest): Either[ProviderError, Message.Assistant]

  /** As [[complete]], telling `onDelta` each piece of the response as it is generated, in
    * order; the result is the whole response, as [[complete]] returns it. A provider that
    * cannot stream tells `onDelta` the whole reasoning and text at once, when complete.
    */
  def stream(
      request: ModelRequest,
      onDelta: Delta => Unit
  ): Either[ProviderError, Message.Assistant] = {
    val response = complete(request)
    response.foreach { m =>
      m.blocks.foreach {
        case AssistantBlock.Reasoning(text, _) if text.nonEmpty => onDelta(Delta.Reasoning(text))
        case AssistantBlock.Text(text) if text.nonEmpty => onDelta(Delta.Text(text))
        case _ => ()
      }
    }
    response
  }
}

/** A piece of a response, as it is generated. */
enum Delta {

  /** More of the reply's text. */
  case Text(text: String)

  /** More of the model's reasoning, before or between its text. */
  case Reasoning(text: String)
}

/** A model call's input: the system prompt, then `messages` in order. */
final case class ModelRequest(system: String, messages: Vector[Message])

/** A model call that produced no response. */
enum ProviderError {

  /** The provider could not be reached or refused the request. */
  case Unavailable(cause: String)
}
