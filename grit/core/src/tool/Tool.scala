package grit.core.tool

import grit.core.message.AssistantBlock
import grit.core.model.ArgRepair
import grit.core.provider.ToolSchema

/** A tool the loop can offer and run: what the model is told of it, whether a person
  * approves each call, how a call is shown, and what it does. `shown` is what a call acts
  * on, as a transcript shows it after the tool's name ([[Bound.shown]]). `run` captures the
  * capabilities it acts through, so a `Tool[A]^{ws}` can do only what `ws` allows. `run`
  * never throws: every failure is an [[Outcome]].
  */
final class Tool[A](
    val spec: ToolSpec[A],
    val gate: Gate[A],
    shown: A -> String,
    run: A => Outcome
) extends Tool.Offered {

  def name: ToolName = spec.name

  def schema(strict: Boolean): ToolSchema = Gate.described(spec.schema(strict), gate != Gate.Free)

  def entry: ToolSet.Entry =
    ToolSet.Entry(spec.name, spec.does, spec.args.schema(false), gate != Gate.Free, spec.retry)

  private[tool] def bind(
      call: AssistantBlock.ToolCall,
      repairs: Set[ArgRepair]
  ): Either[CallError, Bound^{this}] =
    spec.args.read(call.arguments, repairs) match {
      case Left(error) =>
        val sent = call.arguments match {
          case ujson.Str(raw) => raw
          case json => json.render()
        }
        Left(CallError.BadArgs(spec.name, error, sent.take(CallError.Echoed)))
      case Right(args) =>
        val line = Bound.line(spec.name, shown(args))
        Right(gate match {
          case Gate.Free => new Bound.Free(spec.name, line, () => run(args))
          case Gate.Ask(describe) =>
            new Bound.Gated(spec.name, line, describe(args), () => run(args))
        })
    }
}

object Tool {

  /** A stand-in for `entry`, a tool a turn recorded that this build no longer has: offered
    * under its recorded name and schema, asking first when it did, so a replayed turn takes
    * the steps it took; every call of it is answered that the tool is gone, and nothing runs.
    */
  def gone(entry: ToolSet.Entry): Offered = {
    val spec = ToolSpec(entry.name, entry.does, Args.raw(entry.parameters), entry.retry)
    val goneOutcome =
      Outcome.Failed(s"The tool ${ToolName.value(entry.name)} is gone; nothing ran.")
    val gate: Gate[ujson.Value] =
      if (entry.asks)
        Gate.Ask(_ => s"${ToolName.value(entry.name)} is gone: approving it runs nothing.")
      else Gate.Free
    new Tool[ujson.Value](spec, gate, _ => "", _ => goneOutcome)
  }

  /** A tool whatever its arguments' type, as a [[Toolbox]] holds it. */
  sealed trait Offered {
    def name: ToolName

    /** What a request shows the model of it: [[ToolSpec.schema]], its description followed
      * by [[Gate.AsksFirst]] when it asks first.
      */
    def schema(strict: Boolean): ToolSchema

    /** What a turn's tool set records of it ([[ToolSet]]). */
    def entry: ToolSet.Entry

    /** `call`, which names this tool, read against it with `repairs`. */
    private[tool] def bind(
        call: AssistantBlock.ToolCall,
        repairs: Set[ArgRepair]
    ): Either[CallError, Bound^{this}]
  }
}

/** The half of a tool that says what it is and never runs it: what the model is told of it,
  * whether a person approves each call, and how a call is shown. An engine offers it and
  * binds calls to it ([[Bound.Hosted]]); an edge runs them, with the [[Tool]] [[over]] makes
  * from the same description, so both read arguments with one spec.
  */
final class Hosted[A](val spec: ToolSpec[A], val gate: Gate[A], shown: A -> String)
    extends Tool.Offered {

  def name: ToolName = spec.name

  def schema(strict: Boolean): ToolSchema = Gate.described(spec.schema(strict), gate != Gate.Free)

  def entry: ToolSet.Entry =
    ToolSet.Entry(spec.name, spec.does, spec.args.schema(false), gate != Gate.Free, spec.retry)

  /** This tool, run by `run`. */
  def over(run: A => Outcome): Tool[A]^{run} = new Tool(spec, gate, shown, run)

  private[tool] def bind(
      call: AssistantBlock.ToolCall,
      repairs: Set[ArgRepair]
  ): Either[CallError, Bound^{this}] =
    spec.args.read(call.arguments, repairs) match {
      case Left(error) =>
        val sent = call.arguments match {
          case ujson.Str(raw) => raw
          case json => json.render()
        }
        Left(CallError.BadArgs(spec.name, error, sent.take(CallError.Echoed)))
      case Right(args) =>
        val line = Bound.line(spec.name, shown(args))
        val ask = gate match {
          case Gate.Free => None
          case Gate.Ask(describe) => Some(describe(args))
        }
        Right(new Bound.Hosted(spec.name, line, ask, call.arguments, repairs, spec.retry))
    }
}
