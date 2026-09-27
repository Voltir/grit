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

  def schema(strict: Boolean): ToolSchema = spec.schema(strict)

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

  /** A tool whatever its arguments' type, as a [[Toolbox]] holds it. */
  sealed trait Offered {
    def name: ToolName

    /** What a request shows the model of it ([[ToolSpec.schema]]). */
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
