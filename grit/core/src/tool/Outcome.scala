package grit.core.tool

import grit.core.id.ToolCallId
import grit.core.message.Message

/** What settling a tool call came to. Every case reaches the model as the call's result
  * ([[result]]), and the loop carries on: none of them fails the turn. A command's non-zero
  * exit is `Done`, the exit code in its text.
  */
enum Outcome {

  /** It ran; `text` is what the model reads. */
  case Done(text: String)

  /** It could not run, or could not finish: an unknown tool, arguments that do not read, a
    * bad path, an `oldText` not found, a timeout (`why` holding the output so far).
    */
  case Failed(why: String)

  /** The person declined it, with their `reason` if they gave one, or did not answer in
    * time.
    */
  case Denied(reason: Option[String])

  /** A crash cut a gated call short. It may have partly run, and is not run again. */
  case Interrupted

  /** The result the model reads for the call `call`: `Done` is not an error, every other
    * case is. `Denied` says the person declined, and why when they said; `Interrupted`
    * says the call may have partly run and should be checked before it is tried again.
    */
  def result(call: ToolCallId): Message.ToolResult = this match {
    case Done(text) => Message.ToolResult(call, text, isError = false)
    case Failed(why) => Message.ToolResult(call, why, isError = true)
    case Denied(None) =>
      Message.ToolResult(call, "The person declined this call; it did not run.", isError = true)
    case Denied(Some(reason)) =>
      Message.ToolResult(
        call,
        s"The person declined this call; it did not run. They said: $reason",
        isError = true
      )
    case Interrupted =>
      Message.ToolResult(
        call,
        "This call was cut short by a crash and may have partly run; it was not run again. " +
          "Check what it changed before trying it again.",
        isError = true
      )
  }
}
