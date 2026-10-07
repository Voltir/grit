package grit.core.tool

import grit.core.id.CallSlot
import grit.core.message.AssistantBlock
import grit.core.model.ArgRepair
import grit.core.place.Place
import grit.core.provider.ToolSchema

/** A tool the loop can offer and run: what the model is told of it, whether a person
  * approves each call, how a call is shown, and what it does. `shown` is what a call acts
  * on, as a transcript shows it after the tool's name ([[Bound.shown]]). `run` captures the
  * capabilities it acts through, so a `Tool[A]^{ws}` can do only what `ws` allows. `run`
  * never throws: every failure is an [[Outcome]]. A tool whose run is told which call it is
  * comes from [[Hosted.calling]].
  */
final class Tool[A] private[tool] (
    val spec: ToolSpec[A],
    val gate: Gate[A],
    shown: A -> String,
    run: (A, CallSlot) => Outcome
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
      case Left(error) => Left(CallError.refused(spec.name, error, call.arguments))
      case Right(args) =>
        val line = Bound.line(spec.name, shown(args))
        Right(gate match {
          case Gate.Free => new Bound.Free(spec.name, line, at => run(args, at))
          case Gate.Ask(describe) =>
            new Bound.Gated(spec.name, line, describe(args), at => run(args, at))
        })
    }
}

object Tool {

  /** A tool of `spec` and `gate`, a call shown by `shown`, run by `run`, which is not told the
    * call it runs.
    */
  def apply[A](
      spec: ToolSpec[A],
      gate: Gate[A],
      shown: A -> String,
      run: A => Outcome
  ): Tool[A]^{run} =
    new Tool(spec, gate, shown, (args: A, _: CallSlot) => run(args))

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
    Tool[ujson.Value](spec, gate, _ => "", _ => goneOutcome)
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
  def over(run: A => Outcome): Tool[A]^{run} = Tool(spec, gate, shown, run)

  /** This tool, run by `run`, which is told the call it runs. */
  def calling(run: (A, CallSlot) => Outcome): Tool[A]^{run} = new Tool(spec, gate, shown, run)

  private[tool] def bind(
      call: AssistantBlock.ToolCall,
      repairs: Set[ArgRepair]
  ): Either[CallError, Bound^{this}] =
    Hosted.bound(spec, gate, shown, call.arguments, repairs, None)
}

object Hosted {

  /** The hosted tool an edge advertised as `entry`, for one the engine has no description of:
    * offered under `entry`'s name, description and schema, its arguments any JSON object (the
    * edge reads them), a call shown as its arguments' compact JSON cut to [[ShownMax]]
    * characters; `None` when `entry` asks first, since nothing describes what a person would
    * be shown. When `entry` writes ([[ToolSet.Entry.writes]]), its schema adds a required
    * [[Writes.Field]] whose choices are the entry's names, and a call naming none of them, or
    * nothing, is [[CallError.Unwritable]]; a bound call carries the named place as its
    * destination ([[Bound.Hosted.destination]]), and its arguments without the field.
    */
  def advertised(entry: ToolSet.Entry): Option[Tool.Offered] =
    Option.unless(entry.asks) {
      val spec = ToolSpec(entry.name, entry.does, Args.raw(entry.parameters), entry.retry)
      val shown: ujson.Value -> String = args => args.render().take(ShownMax)
      entry.writes.fold[Tool.Offered](new Hosted(spec, Gate.Free, shown))(writes =>
        new Writing(spec, Gate.Free, shown, writes)
      )
    }

  /** The most characters a call of an advertised tool shows of its arguments: 120. */
  val ShownMax: Int = 120

  /** A tool an edge runs that writes outside grit to one of `writes`' destinations, described
    * by `spec`, `gate` and `shown`; why not, when `spec` declares [[Writes.Field]] itself.
    */
  def writing[A, D <: caps.Pure](
      spec: ToolSpec[A],
      gate: Gate[A],
      shown: A -> String,
      writes: Writes[D]
  ): Either[String, Writing[A, D]] =
    if (Writes.declared(spec.args.schema(false)))
      Left(
        s"the tool ${ToolName.value(spec.name)}'s arguments declare ${Writes.Field}, which only " +
          "its writes may name"
      )
    else Right(new Writing(spec, gate, shown, writes))

  /** A call of the tool `spec`, `gate` and `shown` describe, its `arguments` read under
    * `repairs`, bound for an edge to run, writing to `destination`.
    */
  private[tool] def bound[A](
      spec: ToolSpec[A],
      gate: Gate[A],
      shown: A -> String,
      arguments: ujson.Value,
      repairs: Set[ArgRepair],
      destination: Option[Place]
  ): Either[CallError, Bound.Hosted] =
    spec.args.read(arguments, repairs) match {
      case Left(error) => Left(CallError.refused(spec.name, error, arguments))
      case Right(args) =>
        val line = Bound.line(spec.name, shown(args))
        val ask = gate match {
          case Gate.Free => None
          case Gate.Ask(describe) => Some(describe(args))
        }
        Right(
          new Bound.Hosted(spec.name, line, ask, arguments, repairs, spec.retry, destination)
        )
    }
}

/** A writing tool's description ([[Hosted.writing]]): what it advertises, its destinations as
  * places. An engine binds a call of it as a hosted one, to the place its [[Writes.Field]]
  * names ([[Bound.Hosted.destination]]); a call naming none of `writes`' names, or nothing, is
  * [[CallError.Unwritable]].
  */
final class Writing[A, D <: caps.Pure] private[tool] (
    spec: ToolSpec[A],
    gate: Gate[A],
    shown: A -> String,
    writes: Writes[D]
) extends Tool.Offered {

  def name: ToolName = spec.name

  def schema(strict: Boolean): ToolSchema = {
    val described = spec.schema(strict)
    Gate.described(
      described.copy(parameters = writes.shown(described.parameters)),
      gate != Gate.Free
    )
  }

  def entry: ToolSet.Entry =
    ToolSet.Entry(
      spec.name,
      spec.does,
      spec.args.schema(false),
      gate != Gate.Free,
      spec.retry,
      Some(writes.placed)
    )

  private[tool] def bind(
      call: AssistantBlock.ToolCall,
      repairs: Set[ArgRepair]
  ): Either[CallError, Bound^{this}] =
    call.arguments.objOpt match {
      case None => Hosted.bound(spec, gate, shown, call.arguments, repairs, None)
      case Some(sent) =>
        writes
          .chosen(spec.name, sent)
          .flatMap((to, rest) =>
            Hosted.bound(spec, gate, shown, rest, repairs, Some(writes.place(to)))
          )
    }
}
