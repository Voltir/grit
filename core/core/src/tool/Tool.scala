package grit.core.tool

import grit.core.id.CallSlot
import grit.core.message.AssistantBlock
import grit.core.model.ArgRepair
import grit.core.place.Place
import grit.core.provider.ToolSchema
import grit.core.store.Reads
import grit.core.visibility.Integrity

/** A tool the loop can offer and run: what the model is told of it, whether a person
  * approves each call, how a call is shown, and what it does. `shown` is what a call acts
  * on, as a transcript shows it after the tool's name ([[Bound.shown]]). `run` captures the
  * capabilities it acts through, so a `Tool[A]^{ws}` can do only what `ws` allows. `run`
  * never throws: every failure is an [[Outcome]]. A tool whose run is told which call it is
  * comes from [[Hosted.calling]]; one that reads grit's store, from [[Hosted.reading]]; one
  * that requires more than its shape does, or whose results are believed above nothing, from a
  * [[Hosted]] declaring so.
  */
final class Tool[A] private[tool] (
    val spec: ToolSpec[A],
    val gate: Gate[A],
    shown: A -> String,
    run: Tool.Run[A]^,
    aim: Option[Tool.Aim],
    requires: Integrity,
    val output: Believed
) extends Tool.Offered {

  def name: ToolName = spec.name

  def schema(strict: Boolean): ToolSchema = Gate.described(spec.schema(strict), gate != Gate.Free)

  def entry: ToolSet.Entry = aim match {
    case None =>
      val parameters = spec.args.schema(false)
      ToolSet.Entry(
        spec.name,
        spec.does,
        parameters,
        gate != Gate.Free,
        spec.retry,
        None,
        requires,
        spec.action
      )
    case Some(Tool.Aim(parameters, writes)) =>
      val asks = gate != Gate.Free
      ToolSet.Entry(
        spec.name,
        spec.does,
        parameters,
        asks,
        spec.retry,
        Some(writes),
        requires,
        spec.action
      )
  }

  private[tool] def bind(
      call: AssistantBlock.ToolCall,
      repairs: Set[ArgRepair]
  ): Either[CallError, Bound^{this}] = {
    val unwritable = for {
      a <- aim
      sent <- call.arguments.objOpt
      error <- a.writes.chosen(spec.name, sent).left.toOption
    } yield error
    unwritable.fold(read(call.arguments, repairs))(Left(_))
  }

  private[tool] def told(
      call: AssistantBlock.ToolCall,
      repairs: Set[ArgRepair],
      destination: Option[Place]
  ): Either[CallError, Bound^{this}] =
    aim match {
      case None =>
        destination.fold(read(call.arguments, repairs))(at =>
          Left(CallError.Misdirected(spec.name, Some(at)))
        )
      case Some(a) =>
        // The name the arguments are read with is one placed at the destination, so the
        // tool is told the destination there, whatever the call sent.
        destination.flatMap(at => a.writes.to.collectFirst { case (name, `at`) => name }) match {
          case None => Left(CallError.Misdirected(spec.name, destination))
          case Some(name) =>
            val aimed = call.arguments.objOpt.fold(call.arguments)(sent =>
              ujson.Obj.from(
                Writes.without(sent).value.iterator ++ Iterator(Writes.Field -> ujson.Str(name))
              )
            )
            read(aimed, repairs)
        }
    }

  private def read(
      arguments: ujson.Value,
      repairs: Set[ArgRepair]
  ): Either[CallError, Bound^{this}] =
    spec.args.read(arguments, repairs) match {
      case Left(error) => Left(CallError.refused(spec.name, error, arguments))
      case Right(args) =>
        val line = Bound.line(spec.name, shown(args))
        val ask = gate match {
          case Gate.Free => None
          case Gate.Ask(describe, within) => Some((describe(args), within))
        }
        Right(run match {
          case plain: Tool.Plain[A] =>
            ask.fold(new Bound.Free(spec.name, line, output, at => plain(args, at))) {
              (shown, within) =>
                new Bound.Gated(spec.name, line, shown, within, output, at => plain(args, at))
            }
          case reading: Tool.Reading[A] =>
            val run = (at: CallSlot, reads: Reads^) => reading(args, at, reads)
            new Bound.Reading(spec.name, line, ask, output, run)
        })
    }
}

object Tool {

  /** A tool of `spec` and `gate`, a call shown by `shown`, its results believed at `output`
    * when a call does not say, run by `run`, which is not told the call it runs; it requires
    * nothing.
    */
  def apply[A](
      spec: ToolSpec[A],
      gate: Gate[A],
      shown: A -> String,
      output: Believed,
      run: A => Outcome
  ): Tool[A]^{run} =
    new Hosted(spec, gate, shown, output).over(run)

  /** A stand-in for `entry`, a tool a turn recorded that this build no longer has: offered
    * under its recorded name and schema, asking first when it did, writing to the
    * destinations it recorded, and requiring what it did, so its [[Offered.entry]] is `entry`
    * and a replayed turn takes the steps it took; every call of it is answered that the tool
    * is gone, and nothing runs.
    */
  def gone(entry: ToolSet.Entry): Offered = {
    val spec =
      ToolSpec(entry.name, entry.does, Args.raw(entry.parameters), entry.retry, entry.action)
    val goneOutcome =
      Outcome.Failed(s"The tool ${ToolName.value(entry.name)} is gone; nothing ran.")
    val gate: Gate[ujson.Value] =
      if (entry.asks)
        Gate.Ask(_ => s"${ToolName.value(entry.name)} is gone: approving it runs nothing.")
      else Gate.Free
    entry.writes.fold[Offered](
      new Hosted(spec, gate, _ => "", entry.requires, Believed.Default).over(_ => goneOutcome)
    )(writes =>
      new Writing(spec, gate, _ => "", writes, entry.requires).over((_, _) => goneOutcome)
    )
  }

  /** How a tool runs: told its call ([[Plain]]), or handed reads too ([[Reading]]). */
  private[tool] sealed trait Run[A]

  private[tool] final class Plain[A](run: (A, CallSlot) => Outcome) extends Run[A] {
    def apply(args: A, at: CallSlot): Outcome = run(args, at)
  }

  private[tool] final class Reading[A](run: (A, CallSlot, Reads^) => Outcome) extends Run[A] {
    def apply(args: A, at: CallSlot, reads: Reads^): Outcome = run(args, at, reads)
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

    /** What its results are believed at when a call does not say, at no service
      * ([[Believed]]).
      */
    def output: Believed

    /** `call`, which names this tool, read against it with `repairs`. */
    private[tool] def bind(
        call: AssistantBlock.ToolCall,
        repairs: Set[ArgRepair]
    ): Either[CallError, Bound^{this}]

    /** `call`, which names this tool, sent to an edge as a request to write to
      * `destination`, read against it with `repairs` ([[Toolbox.requested]]).
      */
    private[tool] def told(
        call: AssistantBlock.ToolCall,
        repairs: Set[ArgRepair],
        destination: Option[Place]
    ): Either[CallError, Bound^{this}]
  }

  /** What a tool that writes outside grit records of itself ([[Writing.over]]): its
    * parameters without [[Writes.Field]], and its destinations as places.
    */
  private[tool] final case class Aim(parameters: ujson.Value, writes: Writes[Place])
}

/** The half of a tool that says what it is and never runs it: what the model is told of it,
  * whether a person approves each call, how a call is shown, what a call requires, and what
  * its results are believed at. An engine offers it and binds calls to it ([[Bound.Hosted]]);
  * an edge runs them, with the [[Tool]] [[over]] makes from the same description, so both read
  * arguments with one spec. As described, it requires nothing.
  */
final class Hosted[A] private[tool] (
    val spec: ToolSpec[A],
    val gate: Gate[A],
    shown: A -> String,
    requires: Integrity,
    val output: Believed
) extends Tool.Offered {

  /** A tool described by `spec`, `gate` and `shown`, its results believed at `output` when a
    * call does not say.
    */
  def this(spec: ToolSpec[A], gate: Gate[A], shown: A -> String, output: Believed) =
    this(spec, gate, shown, Integrity.Untrusted, output)

  def name: ToolName = spec.name

  def schema(strict: Boolean): ToolSchema = Gate.described(spec.schema(strict), gate != Gate.Free)

  def entry: ToolSet.Entry = {
    val parameters = spec.args.schema(false)
    ToolSet.Entry(
      spec.name,
      spec.does,
      parameters,
      gate != Gate.Free,
      spec.retry,
      None,
      requires,
      spec.action
    )
  }

  /** This hosted tool, its calls bound only when stamped at `integrity` or above. */
  def requiring(integrity: Integrity): Hosted[A] =
    new Hosted(spec, gate, shown, integrity, output)

  /** This tool, run by `run`. */
  def over(run: A => Outcome): Tool[A]^{run} =
    calling((args, _) => run(args))

  /** This tool, run by `run`, which is told the call it runs. */
  def calling(run: (A, CallSlot) => Outcome): Tool[A]^{run} =
    new Tool(spec, gate, shown, new Tool.Plain(run), None, requires, output)

  /** This tool, run by `run`, which is told the call it runs and handed, at each call, reads
    * for that call's turn as the turn hands them ([[Bound.Reading]]). An edge does not run it.
    */
  def reading(run: (A, CallSlot, Reads^) => Outcome): Tool[A]^{run} =
    new Tool(spec, gate, shown, new Tool.Reading(run), None, requires, output)

  private[tool] def bind(
      call: AssistantBlock.ToolCall,
      repairs: Set[ArgRepair]
  ): Either[CallError, Bound^{this}] =
    Hosted.bound(spec, gate, shown, output, call.arguments, repairs, None)

  private[tool] def told(
      call: AssistantBlock.ToolCall,
      repairs: Set[ArgRepair],
      destination: Option[Place]
  ): Either[CallError, Bound^{this}] =
    bind(call, repairs)
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
      val spec =
        ToolSpec(entry.name, entry.does, Args.raw(entry.parameters), entry.retry, entry.action)
      val shown: ujson.Value -> String = args => args.render().take(ShownMax)
      entry.writes.fold[Tool.Offered](
        new Hosted(spec, Gate.Free, shown, entry.requires, Believed.Default)
      )(writes => new Writing(spec, Gate.Free, shown, writes, entry.requires))
    }

  /** The most characters a call of an advertised tool shows of its arguments: 120. */
  val ShownMax: Int = 120

  /** A tool an edge runs that writes outside grit to one of `writes`' destinations, described
    * by `spec`, `gate` and `shown`, requiring what its shape does ([[ToolSet.Entry.byShape]]);
    * why not, when `spec` declares [[Writes.Field]] itself.
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
    else Right(new Writing(spec, gate, shown, writes, ToolSet.Entry.byShape(Some(writes.placed))))

  /** A call of the tool `spec`, `gate`, `shown` and `output` describe, its `arguments` read
    * under `repairs`, bound for an edge to run, writing to `destination`.
    */
  private[tool] def bound[A](
      spec: ToolSpec[A],
      gate: Gate[A],
      shown: A -> String,
      output: Believed,
      arguments: ujson.Value,
      repairs: Set[ArgRepair],
      destination: Option[Place]
  ): Either[CallError, Bound.Hosted] =
    spec.args.read(arguments, repairs) match {
      case Left(error) => Left(CallError.refused(spec.name, error, arguments))
      case Right(args) =>
        val line = Bound.line(spec.name, shown(args))
        val (ask, within) = gate match {
          case Gate.Free => (None, Gate.AnswerWithin)
          case Gate.Ask(describe, within) => (Some(describe(args)), within)
        }
        Right(
          new Bound.Hosted(
            spec.name,
            line,
            ask,
            within,
            arguments,
            repairs,
            spec.retry,
            destination,
            output
          )
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
    writes: Writes[D],
    requires: Integrity
) extends Tool.Offered {

  def name: ToolName = spec.name

  // An edge runs it: its results are believed at its service's declared output.
  def output: Believed = Believed.Default

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
      Some(writes.placed),
      requires,
      spec.action
    )

  private[tool] def bind(
      call: AssistantBlock.ToolCall,
      repairs: Set[ArgRepair]
  ): Either[CallError, Bound^{this}] =
    call.arguments.objOpt match {
      case None => Hosted.bound(spec, gate, shown, output, call.arguments, repairs, None)
      case Some(sent) =>
        writes
          .chosen(spec.name, sent)
          .flatMap((to, rest) =>
            Hosted.bound(spec, gate, shown, output, rest, repairs, Some(writes.place(to)))
          )
    }

  private[tool] def told(
      call: AssistantBlock.ToolCall,
      repairs: Set[ArgRepair],
      destination: Option[Place]
  ): Either[CallError, Bound^{this}] =
    bind(call, repairs)

  /** This tool, run by `run`, told the destination its request was checked to write to
    * ([[grit.core.edge.ToolRequest.destination]]), whatever the call's arguments hold for
    * [[Writes.Field]]: a request holding none, or one at which this tool has no destination,
    * is [[CallError.Misdirected]] and does not run ([[Toolbox.requested]]).
    */
  def over(run: (A, D) => Outcome): Tool[(A, D)]^{run} = {
    val aimed: Gate[(A, D)] = gate match {
      case Gate.Free => Gate.Free
      case Gate.Ask(describe, within) => Gate.Ask(sent => describe(sent._1), within)
    }
    new Tool[(A, D)](
      ToolSpec(spec.name, spec.does, Args.written(spec.args, writes), spec.retry, spec.action),
      aimed,
      sent => shown(sent._1),
      new Tool.Plain((sent, _) => run(sent._1, sent._2)),
      Some(Tool.Aim(spec.args.schema(false), writes.placed)),
      requires,
      Believed.Default
    )
  }
}
