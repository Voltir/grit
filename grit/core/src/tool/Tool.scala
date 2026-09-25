package grit.core.tool

import grit.core.message.AssistantBlock
import grit.core.provider.ToolSchema

/** A tool the loop can offer and run: what the model is told of it, whether a person
  * approves each call, and what a call does. `run` captures the capabilities it acts
  * through, so a `Tool[A]^{ws}` can do only what `ws` allows. `run` never throws: every
  * failure is an [[Outcome]].
  */
final class Tool[A](val spec: ToolSpec[A], val gate: Gate[A], run: A => Outcome)
    extends Tool.Offered {

  def name: ToolName = spec.name

  def schema(strict: Boolean): ToolSchema = spec.schema(strict)

  private[tool] def bind(call: AssistantBlock.ToolCall): Either[CallError, Bound^{this}] =
    spec.args.read(call.arguments) match {
      case Left(error) =>
        val sent = call.arguments match {
          case ujson.Str(raw) => raw
          case json => json.render()
        }
        Left(CallError.BadArgs(spec.name, error, sent.take(CallError.Echoed)))
      case Right(args) =>
        Right(gate match {
          case Gate.Free => new Bound.Free(spec.name, () => run(args))
          case Gate.Ask(describe) => new Bound.Gated(spec.name, describe(args), () => run(args))
        })
    }
}

object Tool {

  /** A tool whatever its arguments' type, as a [[Toolbox]] holds it. */
  sealed trait Offered {
    def name: ToolName

    /** What a request shows the model of it ([[ToolSpec.schema]]). */
    def schema(strict: Boolean): ToolSchema

    /** `call`, which names this tool, read against it. */
    private[tool] def bind(call: AssistantBlock.ToolCall): Either[CallError, Bound^{this}]
  }
}
