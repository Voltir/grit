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
    * cannot stream tells `onDelta` the whole reasoning, text and each call, in the
    * response's order, when complete.
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
        case AssistantBlock.ToolCall(_, name, _) => onDelta(Delta.Calling(name))
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

  /** The model has begun a call of the tool `name`, as it sent the name (possibly no
    * offered tool's); the call's arguments may still be generating. Once per call.
    */
  case Calling(name: String)
}

/** A model call's input: the system prompt, then `messages` in order, and the `tools` the
  * model may call as `use` allows. With no tools the model can only answer in text.
  */
final case class ModelRequest(
    system: String,
    messages: Vector[Message],
    tools: Vector[ToolSchema] = Vector.empty,
    use: ToolUse = ToolUse.Auto
)

/** A tool the model may call, as a request shows it: its `name`, what it is for, and a JSON
  * Schema of its arguments (`parameters`, an object schema). `strict` asks the provider to
  * hold the model's arguments to that schema, which must then meet the provider's strict-mode
  * rules. Built from a [[grit.core.tool.ToolSpec]].
  */
final case class ToolSchema(
    name: String,
    description: String,
    parameters: ujson.Value,
    strict: Boolean = false
)

/** Whether the model may call a request's tools. */
enum ToolUse {

  /** It may call one or more of them, or answer in text. */
  case Auto

  /** It may not call any, and answers in text. The tools are still sent: a request whose
    * messages hold tool calls names the tools they used.
    */
  case Off

  /** It must call one of them: OpenRouter's `tool_choice: "required"`
    * ([[grit.core.model.StrictSchemas.WhenRequired]] is measured under it).
    */
  case Required
}

/** A model call that produced no response. `cause` says why, on one line or more. */
enum ProviderError {

  /** The same request may be answered if sent again: the provider could not be reached or
    * timed out, was overloaded or limited the rate (HTTP 408, 429 or 5xx, or a model error
    * with such a code), or cut the response off before its end.
    */
  case Unavailable(cause: String)

  /** Sending the same request again would fail the same way: the provider refused it (any
    * other HTTP status), its response could not be read, or the model reported an error
    * that is not one of [[Unavailable]]'s.
    */
  case Refused(cause: String)

  def cause: String
}
