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

  /** The person declined it, with their `reason` if they gave one. */
  case Declined(reason: Option[String])

  /** Nobody answered in time whether it may run, so it did not. */
  case Unanswered

  /** A crash cut a gated call short. It may have partly run, and is not run again. */
  case Interrupted

  /** The result the model reads for the call `call`: `Done` is not an error, every other
    * case is. `Declined` and `Unanswered` each begin with their name ("Declined: …",
    * "Unanswered: …"), a declined one ending with the person's reason when they gave one;
    * `Interrupted` says the call may have partly run and should be checked before it is
    * tried again.
    */
  def result(call: ToolCallId): Message.ToolResult = this match {
    case Done(text) => Message.ToolResult(call, text, isError = false)
    case Failed(why) => Message.ToolResult(call, why, isError = true)
    case Declined(reason) =>
      val said = reason.fold("")(r => s" Their reason: $r")
      Message.ToolResult(
        call,
        s"Declined: the person declined this call; it did not run.$said",
        isError = true
      )
    case Unanswered =>
      Message.ToolResult(
        call,
        "Unanswered: nobody answered in time; the call did not run.",
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
