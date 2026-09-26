package grit.core.tool

import grit.core.approval.Approval
import grit.core.message.AssistantBlock
import grit.core.model.NameRepair
import grit.core.provider.ToolSchema

/** The tools offered on one model call, their names distinct, each acting only through the
  * capabilities `C`: a `Toolbox[{ws}]` cannot edit or run a command unless `ws` can.
  */
final class Toolbox[+C^] private (tools: Vector[Tool.Offered^{C}]) {

  /** The tools as a request shows them, in the order given to [[Toolbox.of]]. */
  def schemas(strict: Boolean): Vector[ToolSchema] = tools.map(_.schema(strict))

  /** The names offered, in order. */
  def names: Vector[ToolName] = tools.map(_.name)

  /** This toolbox with `tool`, which acts through no capability, offered first; `Left` when
    * one here has its name.
    */
  def including(tool: Tool.Offered): Either[DuplicateName, Toolbox[C]] =
    Toolbox.of[C]((tool +: tools)*)

  /** `call` read against the tool it names ([[Toolbox.named]]), with `repairs`, ready to
    * run; or why it cannot be: no tool has that name, or its arguments do not read.
    */
  def bind(call: AssistantBlock.ToolCall, repairs: Repairs): Either[CallError, Bound^{C}] =
    tools.find(t => ToolName.value(t.name) == Toolbox.named(call.name, repairs.names)) match {
      case None => Left(CallError.Unknown(call.name, names))
      case Some(tool) => tool.bind(call, repairs.args)
    }
}

object Toolbox {

  /** The tool name a call sent as `sent` means: under [[NameRepair.HarmonyCut]], `sent` up to
    * its first `<|`, which no tool name holds (gpt-oss leaks its harmony tokens into the name
    * it sends: `read<|channel|>commentary`); under [[NameRepair.AsSent]], `sent`.
    */
  def named(sent: String, repair: NameRepair): String = repair match {
    case NameRepair.AsSent => sent
    case NameRepair.HarmonyCut =>
      sent.indexOf("<|") match {
        case -1 => sent
        case at => sent.take(at)
      }
  }

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

/** A call whose tool was found and whose arguments read, ready to run: [[Bound.Free]] as it
  * is, [[Bound.Gated]] only with a person's [[Approval]] in hand.
  */
sealed trait Bound {
  def tool: ToolName

  /** The call in one line, as a transcript shows it: the tool's name, then what the call
    * acts on (a path, a pattern, a command), its line breaks shown as spaces.
    */
  def shown: String
}

object Bound {

  /** A call of a tool that runs without asking. */
  final class Free private[tool] (val tool: ToolName, val shown: String, run: () => Outcome)
      extends Bound {

    def apply(): Outcome = run()
  }

  /** A call of a tool a person approves first; `ask` is what they are shown of it, which
    * may run to many lines ([[Gate.Ask]]).
    */
  final class Gated private[tool] (
      val tool: ToolName,
      val shown: String,
      val ask: String,
      run: () => Outcome
  ) extends Bound {

    /** Runs the call when `approval` is [[Approval.Approved]]. Otherwise it does not run:
      * [[Outcome.Denied]] with the person's reason when they declined, or [[Unanswered]]
      * when the wait timed out.
      */
    def apply(approval: Approval): Outcome = approval match {
      case Approval.Approved => run()
      case Approval.Declined(reason) => Outcome.Denied(reason)
      case Approval.TimedOut => Outcome.Denied(Some(Unanswered))
    }
  }

  /** [[Bound.shown]] for a call of `tool` that acts on `on`. */
  private[tool] def line(tool: ToolName, on: String): String = {
    val flat = on.trim.replaceAll("\\s*\\R\\s*", " ")
    if (flat.isEmpty) ToolName.value(tool) else s"${ToolName.value(tool)} $flat"
  }

  /** The reason a call whose approval timed out is denied. */
  val Unanswered = "No answer came in time."
}
