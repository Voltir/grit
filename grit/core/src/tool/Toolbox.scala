package grit.core.tool

import grit.core.message.AssistantBlock
import grit.core.provider.ToolSchema

/** The tools offered on one model call, their names distinct, each acting only through the
  * capabilities `C`: a `Toolbox[{ws}]` cannot edit or run a command unless `ws` can.
  */
final class Toolbox[C^] private (tools: Vector[Tool.Offered^{C}]) {

  /** The tools as a request shows them, in the order given to [[Toolbox.of]]. */
  def schemas(strict: Boolean): Vector[ToolSchema] = tools.map(_.schema(strict))

  /** The names offered, in order. */
  def names: Vector[ToolName] = tools.map(_.name)

  /** `call` read against the tool it names, ready to run; or why it cannot be: no tool
    * has that name, or its arguments do not read.
    */
  def bind(call: AssistantBlock.ToolCall): Either[CallError, Bound^{C}] =
    tools.find(t => ToolName.value(t.name) == call.name) match {
      case None => Left(CallError.Unknown(call.name, names))
      case Some(tool) => tool.bind(call)
    }
}

object Toolbox {

  /** A toolbox of `tools`, offered in this order; `Left` names the first name repeated. */
  def of[C^](tools: Tool.Offered^{C}*): Either[DuplicateName, Toolbox[C]] = {
    val all = tools.toVector
    val names = all.map(_.name)
    names.diff(names.distinct).headOption match {
      case Some(repeated) => Left(DuplicateName(repeated))
      case None => Right(new Toolbox(all))
    }
  }
}

/** A call whose tool was found and whose arguments read. `ask` is what a person is shown
  * to approve it when its tool is gated; `None` when it runs without asking.
  */
final class Bound private[tool] (
    val tool: ToolName,
    val ask: Option[String],
    run: () => Outcome
) {

  /** Runs the call. */
  def apply(): Outcome = run()
}
