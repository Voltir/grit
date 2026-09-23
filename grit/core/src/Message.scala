package grit.core

/** One message in a conversation, independent of any provider's wire format.
  * The system prompt is not a message: it is configuration, placed by the turn.
  */
enum Message {

  /** What the human (or a trigger) said. */
  case User(text: String)

  /** One model response: its blocks in order, why it stopped, and what it
    * cost. `model` is the id the provider says served it.
    */
  case Assistant(
      blocks: Vector[AssistantBlock],
      stop: StopReason,
      usage: Usage,
      model: String
  )

  /** The outcome of running the tool call `callId`. An error is still a result:
    * the next model call sees it.
    */
  case ToolResult(callId: ToolCallId, content: String, isError: Boolean)
}

/** A block of an assistant message. */
enum AssistantBlock {
  case Text(text: String)

  /** The model's reasoning. `text` is readable, possibly empty or a summary;
    * `replay` is the provider's own form, which must be sent back verbatim on
    * later calls within the same turn and is otherwise opaque.
    */
  case Reasoning(text: String, replay: Option[ujson.Value])

  case ToolCall(id: ToolCallId, name: String, arguments: ujson.Value)
}

/** Why a model response ended. `Other` keeps a reason grit has no case for. */
enum StopReason {
  case EndTurn
  case ToolUse
  case MaxTokens
  case ContentFilter
  case Other(raw: String)
}
