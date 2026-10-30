package grit.core.tool

import grit.core.place.Place
import grit.core.visibility.Integrity

/** Why a tool call cannot be run. `message` is what the model reads as the call's result. */
enum CallError {

  /** No tool offered is named `name`; `offered` are those that are, in order. */
  case Unknown(name: String, offered: Vector[ToolName])

  /** `tool`'s arguments did not read; `sent` is what the model sent, cut to
    * [[CallError.Echoed]] characters: the JSON, or, when a JSON string stands for arguments
    * that were not JSON, that text.
    */
  case BadArgs(tool: ToolName, error: ArgsError, sent: String)

  /** `tool` writes outside grit and its call named no place it may write to here: `sent` is the
    * name it gave, cut to [[CallError.Echoed]] characters (`None` for none), `offered` the
    * names it may give, in order.
    */
  case Unwritable(tool: ToolName, sent: Option[String], offered: Vector[String])

  /** A request sent to an edge to write with `tool` to `destination` (`None` for nowhere),
    * where the edge's `tool` has no destination; it does not run.
    */
  case Misdirected(tool: ToolName, destination: Option[Place])

  /** It was stamped at `at`, below what `tool` requires, `requires`: this turn read something
    * believed less than the tool needs, and it does not run. `plainly` is what a person can no
    * longer have done by it ([[ToolSet.Entry.plainly]]).
    */
  case Withheld(tool: ToolName, plainly: String, requires: Integrity, at: Integrity)

  /** What is wrong, then the tools there are, or what was sent; a withheld call, in words the
    * model may repeat to a person, naming neither level.
    */
  def message: String = this match {
    case Unknown(name, offered) =>
      val names = offered.map(n => s"`${ToolName.value(n)}`").mkString(", ")
      if (offered.isEmpty) s"There is no tool named `$name`, and no tool is offered."
      else s"There is no tool named `$name`; the tools are $names."
    case BadArgs(tool, error, sent) =>
      s"The call to `${ToolName.value(tool)}` was not run: ${error.message} You sent: $sent"
    case Unwritable(tool, sent, offered) =>
      val not = sent.fold("")(s => s", not $s")
      s"The call to `${ToolName.value(tool)}` was not run: `${Writes.Field}` must be one of " +
        s"${offered.mkString(", ")}, the places it may write to from here$not. Nothing was sent."
    case Misdirected(tool, destination) =>
      destination.fold(s"${ToolName.value(tool)} was told no place to write to; it did not run.")(
        at => s"${ToolName.value(tool)} does not write to ${at.written} here; it did not run."
      )
    case Withheld(_, plainly, _, _) =>
      s"You can't $plainly right now, because of something you read; nothing was done, so " +
        "answer without it."
  }

  /** This error as the call's outcome: [[Outcome.Failed]] with [[message]]. */
  def outcome: Outcome = Outcome.Failed(message)
}

object CallError {

  /** The most characters of a call's arguments that [[CallError.BadArgs]] echoes. */
  val Echoed = 500

  /** [[BadArgs]] for `tool`'s `arguments`, which did not read for `error`. */
  private[tool] def refused(tool: ToolName, error: ArgsError, arguments: ujson.Value): CallError = {
    val sent = arguments match {
      case ujson.Str(raw) => raw
      case json => json.render()
    }
    BadArgs(tool, error, sent.take(Echoed))
  }
}

/** Why tools cannot be offered as one set ([[ToolSet.of]], [[Toolbox.of]]): the tool `name`
  * clashes, as `message` says for a person.
  */
sealed trait SetRefusal {
  def name: ToolName
  def message: String
}

/** Two tools offered together share the name `name`. */
final case class DuplicateName(name: ToolName) extends SetRefusal {
  def message: String = s"the tool ${ToolName.value(name)} is offered twice"
}

/** The tool `name`'s parameters declare [[Writes.Field]], which only a writing tool's
  * destinations name: a set holding it could not be read back from its stored form.
  */
final case class DeclaresField(name: ToolName) extends SetRefusal {
  def message: String =
    s"the tool ${ToolName.value(name)}'s parameters declare ${Writes.Field}, which only its writes may name"
}
