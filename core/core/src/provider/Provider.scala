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
  * model may call as `use` allows. With no tools the model can only answer in text. What goes
  * in `system` and what in `messages`, and how to word and fence it: the `grit-model-text` skill.
  *
  * `repeats` says which of `messages`, from the first, later requests repeat: with `tools` and
  * `system`, each of its counts is a prefix a provider may cache.
  */
final case class ModelRequest(
    system: String,
    messages: Vector[Message],
    tools: Vector[ToolSchema] = Vector.empty,
    use: ToolUse = ToolUse.Auto,
    repeats: Repeats = Repeats.Zero
)

/** How many of a request's messages, from the first, later requests repeat unchanged: the first
  * `task` hold what its task is (a boundary's brief, nearby sections and documents), the first
  * `established` also what earlier turns of its window established, and the first `stable`
  * every message the caller's later requests in the same exchange repeat. A count past the end
  * of the messages covers every one. A count that overclaims costs a cache write, never a
  * different reply.
  */
final case class Repeats private (task: Int, established: Int, stable: Int)

object Repeats {

  /** No message repeated: only `tools` and `system`. */
  val Zero: Repeats = new Repeats(0, 0, 0)

  /** These counts, each at least 0 and at most the next: `task` cut to `established`, and
    * `established` to `stable`.
    */
  def of(task: Int, established: Int, stable: Int): Repeats = {
    val s = stable.max(0)
    val e = established.max(0).min(s)
    new Repeats(task.max(0).min(e), e, s)
  }

  /** The first `count` messages repeated, with no task or established part named. */
  def stable(count: Int): Repeats = of(0, 0, count)
}

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

  /** It is asked to call one of them: forced where the provider can (OpenRouter's
    * `tool_choice: "required"`, which [[grit.core.model.StrictSchemas.WhenRequired]] is
    * measured under). Anthropic's current models refuse a forced choice, so there it is
    * `auto`, and a reply may make no call.
    */
  case Required

  /** It is asked to call the tool named `name`, and no other: forced where the provider can
    * name one tool. Where it cannot, as on Anthropic's models, whose thinking refuses a forced
    * choice, it is [[Auto]], and the call is asked for only in the request's words: a reply may
    * call another tool, or none.
    */
  case Only(name: String)
}

/** A model call that produced no response. `cause` says why, on one line or more. */
enum ProviderError {

  /** The same request may be answered if sent again: the provider could not be reached or
    * timed out, was overloaded or limited the rate (HTTP 408, 429 or 5xx, or a model error
    * with such a code), or cut the response off before its end.
    */
  case Unavailable(cause: String)

  /** Sending the same request again would fail the same way: the provider refused it (any
    * other HTTP status), its response could not be read, the model declined to answer, or
    * the model reported an error that is not one of [[Unavailable]]'s.
    */
  case Refused(cause: String)

  def cause: String
}
