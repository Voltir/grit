package grit.core.tool

/** Why a tool call cannot be run. `message` is what the model reads as the call's result. */
enum CallError {

  /** No tool offered is named `name`; `offered` are those that are, in order. */
  case Unknown(name: String, offered: Vector[ToolName])

  /** `tool`'s arguments did not read; `sent` is what the model sent, cut to
    * [[CallError.Echoed]] characters: the JSON, or, when a JSON string stands for arguments
    * that were not JSON, that text.
    */
  case BadArgs(tool: ToolName, error: ArgsError, sent: String)

  /** What is wrong, then the tools there are, or what was sent. */
  def message: String = this match {
    case Unknown(name, offered) =>
      val names = offered.map(n => s"`${ToolName.value(n)}`").mkString(", ")
      if (offered.isEmpty) s"There is no tool named `$name`, and no tool is offered."
      else s"There is no tool named `$name`; the tools are $names."
    case BadArgs(tool, error, sent) =>
      s"The call to `${ToolName.value(tool)}` was not run: ${error.message} You sent: $sent"
  }

  /** This error as the call's outcome: [[Outcome.Failed]] with [[message]]. */
  def outcome: Outcome = Outcome.Failed(message)
}

object CallError {

  /** The most characters of a call's arguments that [[CallError.BadArgs]] echoes. */
  val Echoed = 500
}

/** Two tools offered together share the name `name`. */
final case class DuplicateName(name: ToolName)
