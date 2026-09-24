package grit.core.provider

import grit.core.message.Message

/** Capability to call a model. Each call is one request with no memory of earlier ones:
  * everything the model sees is in the request.
  */
trait Provider extends caps.SharedCapability {

  /** One model response to `request`. Called once; a caller that wants a retry makes it. */
  def complete(request: ModelRequest): Either[ProviderError, Message.Assistant]
}

/** A model call's input: the system prompt, then `messages` in order. */
final case class ModelRequest(system: String, messages: Vector[Message])

/** A model call that produced no response. */
enum ProviderError {

  /** The provider could not be reached or refused the request. */
  case Unavailable(cause: String)
}
