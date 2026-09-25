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

/** A model call's input: the system prompt, then `messages` in order, and the `tools` the
  * model may call as `use` allows. With no tools the model can only answer in text.
  */
final case class ModelRequest(
    system: String,
    messages: Vector[Message],
    tools: Vector[Tool] = Vector.empty,
    use: ToolUse = ToolUse.Auto
)

/** A tool the model may call: its `name`, what it is for, and a JSON Schema of its
  * arguments (`parameters`, an object schema).
  */
final case class Tool(name: String, description: String, parameters: ujson.Value)

/** Whether the model may call a request's tools. */
enum ToolUse {

  /** It may call one or more of them, or answer in text. */
  case Auto

  /** It may not call any, and answers in text. The tools are still sent: a request whose
    * messages hold tool calls names the tools they used.
    */
  case Off
}

/** A model call that produced no response. */
enum ProviderError {

  /** The provider could not be reached or refused the request. */
  case Unavailable(cause: String)
}
