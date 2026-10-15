package grit.turn

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.act.phase.Calling
import grit.core.approval.Approval
import grit.core.context.Window
import grit.core.durable.{Flow, Observed, Step, StepName}
import grit.core.edge.Permit
import grit.core.id.{EntryId, ToolCallId, TurnRef}
import grit.core.message.Message
import grit.core.place.Place
import grit.core.provider.ToolUse
import grit.core.store.{EntryStore, Tx}
import grit.core.tool.{Bound, ToolName}
import grit.core.visibility.{Integrity, Subject}

import TurnLoop.{Budget, Next, Pending, Round}
import TurnTools.{Kind, Settled, Slot}
import TurnJournal.given

/** A turn's tool loop, as a flow: each round's `gate:n`, its model call
  * `ask:n`, and, for a reply that calls tools ([[TurnLoop.next]]), its `ask:n:record`, its free
  * hosted calls sent to each place together (`call:n:send` to the workspace,
  * `call:n:send:{place}` to a service it reaches), and each call settled in order: in its
  * `call:n:j`, after `call:n:j:approve` and a person's answer for one that asks, and after a
  * hosted call's own steps ([[TurnHosted.settle]]). Until a reply answers, or a step fails. Its
  * bodies act through an [[Env]]; what crosses its rounds is [[Rounds]], on the wire.
  */
private[turn] object TurnRound {

  /** What a turn's rounds act through: each member the body of a step, or what one reads. A
    * call is read against what its round offers, stamped at `at.level` ([[TurnTools.read]]),
    * by each member that takes it, every time: what it binds to never leaves this. It has no
    * capture-set parameter, and everything it hands out is pure or captures it alone, so no
    * runner of a call can reach the loop's state on the wire.
    */
  trait Env {

    /** `gate:n`'s body: round `at.round`'s gate, decided from the turn's start for the first
      * round and from `at.level` after.
      */
    def gate(at: Rounds): Either[TurnFailure, Gated]

    /** The body of round `at.round`'s model call: the model's reply, offered tools as `use`
      * says.
      */
    def call(at: Rounds, use: ToolUse): Either[TurnFailure, Message.Assistant]

    /** `ask:n:record`'s body: `reply`, the reply to `at.round` that called tools, kept, and
      * what each of its `calls` is. Fails, as every member here does, when the tools the round
      * offers no longer make one toolbox, which `record-window` checked in the run that
      * recorded it ([[TurnFlow]]), and as `TurnFailure.Store` when its offer's tool set or
      * prompt, or its route's profile, is not kept ([[TurnHosting.offered]],
      * [[Turn.Routed.picked]]).
      */
    def record(at: Rounds, reply: Message.Assistant, calls: Vector[Pending])(using
        Tx^
    ): Either[TurnFailure, Kept]

    /** `pending`, the call at `slot` of round `at.round`, bound to its hosted tool. Fails, as
      * each member below that binds a call does, with [[TurnFailure.ToolsChanged]] when it
      * binds otherwise than its round recorded.
      */
    def hosted(at: Rounds, slot: Slot, pending: Pending): Either[TurnFailure, Bound.Hosted]

    /** What a person is shown of `call` when its tool asks them first. */
    def shown(call: Call): Either[TurnFailure, String]

    /** The `call:n:j` body of `call`, refused or free: answered with its outcome, unrun, at
      * the turn's level, or run.
      */
    def settle(call: Call): Either[TurnFailure, Settled]

    /** The `call:n:j` body of `call`, gated, answered by `approval`
      * ([[TurnTools.Settling.decide]]).
      */
    def decide(call: Call, approval: Approval): Either[TurnFailure, Settled]

    /** Where the calls' results are kept. */
    def settling: TurnTools.Settling^{this}

    /** The time a step's body dates what it keeps. */
    def now(): Instant

    /** Where hosted calls go. */
    def hosting: TurnHosting
  }

  /** A step's input, of round `round`. */
  trait OfRound {
    def round: Round
  }

  /** A step's input, of the call at `slot`. */
  trait OfCall {
    def slot: Slot
  }

  /** What a turn's loop runs under: `turn`, offering what `offer` names, its calls made as
    * `routed` says over `seen`, its window, as `budget` allows, `asked` the classification its
    * model is asked about when the classifier was unsure; a call that asks a person first
    * waits `answerWithin`.
    */
  final case class Looping(
      turn: TurnRef,
      offer: TurnOffer.Recorded,
      routed: Turn.Routed,
      seen: Window,
      asked: Option[TurnTopics.Classification],
      budget: Budget,
      answerWithin: FiniteDuration
  )

  /** Where a turn's loop, run under `looping`, stands at round `round`: for a gated turn, the
    * gate of each round so far, by its index; and every reply so far, as the model sent it,
    * the first first: the verdict on the message's topic is read from the first and checked
    * against them all.
    */
  final case class Rounds(
      looping: Looping,
      round: Round,
      gates: Map[Int, Gated],
      replies: Vector[Message.Assistant]
  ) extends OfRound {

    /** The turn's level, what its rows are believed at: its first gate's, met with each later
      * one's in turn; [[Integrity.Authority]] before any.
      */
    def level: Integrity =
      gates.values.map(_.level).reduceOption(_.meet(_)).getOrElse(Integrity.Authority)

    private[TurnRound] def gatedBy(gate: Gated): Rounds =
      copy(gates = gates.updated(round.index, gate))
  }

  /** What a turn's loop is allowed: `budget`, its model calls, and `answerWithin`, how long a
    * call that asks a person first waits for them. Recorded with the turn's window, so a
    * rerun keeps the limits its turn started under.
    */
  final case class Limits(budget: Budget, answerWithin: FiniteDuration)

  /** What a round whose reply called tools kept: the reply, as `reply`, and what each of its
    * calls is, in order, as the run that kept it read them.
    */
  final case class Kept(reply: EntryId, kinds: Vector[Kind])

  /** A loop that answered with `answer`, the reply to `at.round` with its calls dropped. */
  final case class Looped(answer: Message.Assistant, at: Rounds)

  /** A loop that ended for `failure`, where `at` says. */
  final case class Stalled(failure: TurnFailure, at: Rounds)

  /** What the model call `ask:round` shows a reader of the turn's history: that round `round`
    * (from 0) was asked.
    */
  final case class Asked(round: Int) extends Observed

  /** The call `pending` of `at.round`'s reply, at `slot`, bound as `kind`; `sent`, whether its
    * round's free hosted calls to its place reached a serving edge.
    */
  final case class Call(at: Rounds, slot: Slot, pending: Pending, kind: Kind, sent: Boolean)
      extends OfCall {
    def id: ToolCallId = pending.call.id

    /** Which steps settle it, by its kind: its own step (`Left`), its own step once a person
      * approves it (`Right(Left)`), or an edge's (`Right(Right)`).
      */
    def routing: Either[Call, Either[Call, Hosting]] = kind match {
      case Kind.Refused | Kind.Free => Left(this)
      case Kind.Gated => Right(Left(this))
      case Kind.Hosted(tool, asks, place) => Right(Right(Hosting(this, tool, asks, place)))
    }
  }

  /** `call`, of a hosted tool `tool` at `place`, which `asks` a person first or not. */
  final case class Hosting(call: Call, tool: ToolName, asks: Boolean, place: Option[Place])
      extends OfCall {
    def slot: Slot = call.slot
  }

  /** The names of the loop's steps, and of a person's wait, and how a name is read back: the
    * one home of each. Round n's model call is `ask:n`, its record `ask:n:record`, its gate
    * `gate:n`; call j of its reply is `call:n:j`, with its phases `call:n:j:{phase}`; its free
    * hosted calls are sent in `call:n:send` to the workspace and `call:n:send:{place}` to a
    * service it reaches.
    */
  object Names {
    val gate: StepName[OfRound] = StepName.keyed[OfRound]("gate")(r => Step.Gate(n(r)).key)
    val ask: StepName[OfRound] = StepName.keyed[OfRound]("ask")(r => Step.Ask(n(r)).key)
    val record: StepName[OfRound] = StepName.keyed[OfRound]("ask")(r => Step.Record(n(r)).key)
    val send: StepName[OfRound] = StepName.keyed[OfRound]("call")(r => Step.Send(n(r), None).key)
    val sendTo: StepName[Sending] =
      StepName.keyed[Sending]("call")(s => Step.Send(n(s), Some(s.place.written)).key)
    val call: StepName[OfCall] = StepName.keyed[OfCall]("call")(c => Step.Call(n(c), j(c)).key)
    val approve: StepName[OfCall] =
      StepName.keyed[OfCall]("call")(c => Step.Approve(n(c), j(c)).key)
    val dispatch: StepName[OfCall] =
      StepName.keyed[OfCall]("call")(c => Step.Dispatch(n(c), j(c)).key)
    val expire: StepName[OfCall] = StepName.keyed[OfCall]("call")(c => Step.Expire(n(c), j(c)).key)
    val abandon: StepName[OfCall] =
      StepName.keyed[OfCall]("call")(c => Step.Abandon(n(c), j(c)).key)

    /** A person's time to answer about a call: a wait's label, never recorded. */
    val answer: StepName[OfCall] = StepName.keyed[OfCall]("call")(c => Step.Wait(n(c), j(c)).key)

    /** A step of the loop, by its round `round` and, for one of a call, the call's `index`. */
    enum Step {
      case Ask(round: Int)
      case Record(round: Int)
      case Gate(round: Int)
      case Send(round: Int, place: Option[String])
      case Call(round: Int, index: Int)
      case Approve(round: Int, index: Int)
      case Wait(round: Int, index: Int)
      case Dispatch(round: Int, index: Int)
      case Expire(round: Int, index: Int)
      case Abandon(round: Int, index: Int)

      /** The name this step is recorded under, or a wait labelled. */
      def name: String = s"$family:$key"

      private def family: String = this match {
        case Ask(_) | Record(_) => ask.family
        case Gate(_) => gate.family
        case _ => call.family
      }

      private[Names] def key: String = this match {
        case Ask(n) => s"$n"
        case Record(n) => s"$n:record"
        case Gate(n) => s"$n"
        case Send(n, None) => s"$n:send"
        case Send(n, Some(place)) => s"$n:send:$place"
        case Step.Call(n, j) => s"$n:$j"
        case Approve(n, j) => s"$n:$j:approve"
        case Wait(n, j) => s"$n:$j:wait"
        case Dispatch(n, j) => s"$n:$j:dispatch"
        case Expire(n, j) => s"$n:$j:expire"
        case Abandon(n, j) => s"$n:$j:abandon"
      }
    }

    object Step {

      /** The step `name` names; `None` for a name that is not the loop's. */
      def of(name: String): Option[Step] =
        name.split(":", 5).toVector match {
          case Vector(f, Num(n)) if f == ask.family => Some(Ask(n))
          case Vector(f, Num(n), "record") if f == ask.family => Some(Record(n))
          case Vector(f, Num(n)) if f == gate.family => Some(Gate(n))
          case Vector(f, Num(n), "send") if f == call.family => Some(Send(n, None))
          case Vector(f, Num(n), "send", place, rest*) if f == call.family =>
            Some(Send(n, Some((place +: rest).mkString(":"))))
          case Vector(f, Num(n), Num(j)) if f == call.family => Some(Step.Call(n, j))
          case Vector(f, Num(n), Num(j), phase) if f == call.family =>
            phase match {
              case "approve" => Some(Approve(n, j))
              case "wait" => Some(Wait(n, j))
              case "dispatch" => Some(Dispatch(n, j))
              case "expire" => Some(Expire(n, j))
              case "abandon" => Some(Abandon(n, j))
              case _ => None
            }
          case _ => None
        }

      private object Num {
        def unapply(s: String): Option[Int] =
          Option.when(s.forall(_.isDigit))(s).flatMap(_.toIntOption)
      }
    }

    private def n(r: OfRound): Int = r.round.index
    private def n(c: OfCall): Int = c.slot.round.index
    private def j(c: OfCall): Int = c.slot.index
  }

  /** The loop from its first round, run under its input: its hosted calls go to the places its
    * offer gives them, sent as its pins say.
    */
  def flow: Flow[Env, Looping, Stalled, Looped] =
    Flow
      .pass[Looping]
      .map(l => Rounds(l, Round.First, Map.empty, Vector.empty))
      .andThen(Flow.loop(round))

  /** What a row `turn` writes is believed at: `believed`, its level once it reached its loop
    * ([[Rounds.level]]), or, for a turn that ended before it, what its entries say
    * ([[EntryStore.believed]]).
    */
  private[turn] def believedAt(entries: EntryStore, turn: TurnRef, believed: Option[Integrity])(
      using Tx^
  ): Either[TurnFailure, Integrity] =
    believed.fold(entries.believed(turn).left.map(TurnFailure.ofStore))(Right(_))

  /** A person asked about `call` in `call:n:j:approve`, shown what its tool asks them, then
    * their answer, waited for as long as its loop gives them ([[Calling.approval]]); why the
    * ask was not kept, when it was not, and nothing waited for.
    */
  private[turn] val approved: Flow[Env, Call, Nothing, Either[TurnFailure, Approval]] = {
    val asked = Step.transact[Env, Call, Either[TurnFailure, EntryId]](
      Names.approve,
      c => Subject.Turn(c.slot.turn)
    )(r =>
      c =>
        (tx: Tx^) ?=>
          r.shown(c)
            .flatMap(TurnTools.ask(r.settling.entries, c.slot, c.id, _, r.now()))
    )
    Flow
      .step(asked)
      .keeping
      .map((c, kept) => kept.map(_ => c))
      .andThen(Calling.approval[Call](Names.answer, _.id, _.at.looping.answerWithin).right)
  }

  /** A reply to `at.round` that called tools, its `calls` to settle before the call `next`. */
  private final case class Settle(
      at: Rounds,
      reply: Message.Assistant,
      calls: Vector[Pending],
      next: Round
  ) extends OfRound {
    def round: Round = at.round
  }

  /** `at.round`'s calls, each at its slot, bound as its kind, before the call `next`. */
  private final case class Slotted(at: Rounds, next: Round, calls: Vector[(Slot, Pending, Kind)])

  /** `calls` of `at.round`'s reply, free and hosted at `place`, sent together. */
  final case class Sending(at: Rounds, place: Place, calls: Vector[(Slot, Pending)])
      extends OfRound {
    def round: Round = at.round
  }

  /** `at.round`'s calls, to settle in order before the call `next`. */
  private final case class Calls(at: Rounds, next: Round, calls: Vector[Call])

  /** A gated call, answered by `approval`. */
  private final case class Decided(call: Call, approval: Approval) extends OfCall {
    def slot: Slot = call.slot
  }

  /** One round: its gate, its call, and, when its reply called tools, those calls settled. */
  private def round: Flow[Env, Rounds, Stalled, Either[Rounds, Looped]] = {
    val gate = Step[Env, Rounds, Either[TurnFailure, Gated]](Names.gate)(r => at => r.gate(at))
    val gating = Flow.decide(gate)((at, g) => stalling(at)(g.map(at.gatedBy)))
    val calling = Step[Env, Rounds, Either[TurnFailure, Message.Assistant]](
      Names.ask,
      at => Some(Asked(at.round.index))
    )(r => at => r.call(at, TurnLoop.use(at.looping.budget, at.round)))
    gating
      .andThen(Flow.decide(calling)(replied))
      .andThen(
        Flow.either(
          settles.map[Either[Rounds, Looped]](Left(_)),
          Flow.pass[Looped].map[Either[Rounds, Looped]](Right(_))
        )
      )
  }

  /** What a round does after its call came to `called`: ends, answers, or settles its calls. */
  private def replied(
      at: Rounds,
      called: Either[TurnFailure, Message.Assistant]
  ): Either[Stalled, Either[Settle, Looped]] =
    called match {
      case Left(failure) => Left(Stalled(failure, at))
      case Right(reply) =>
        val now = at.copy(replies = at.replies :+ reply)
        TurnLoop.next(at.looping.budget, at.round, reply) match {
          case Next.Answer(answer) => Right(Right(Looped(answer, now)))
          case Next.Silent(_) =>
            val said =
              s"the reply to ${Names.Step.Ask(at.round.index).name} said nothing and called no tool"
            Left(Stalled(TurnFailure.Model(said), now))
          case Next.Settle(calls, next) => Right(Left(Settle(now, reply, calls, next)))
        }
    }

  /** A reply's calls settled: the reply kept with what each of its calls is, its free hosted
    * calls sent, and each call settled in order; then the round after.
    */
  private def settles: Flow[Env, Settle, Stalled, Rounds] = {
    val record = Step.transact[Env, Settle, Either[TurnFailure, Kept]](
      Names.record,
      s => Subject.Turn(s.at.looping.turn)
    )(r => s => (tx: Tx^) ?=> r.record(s.at, s.reply, s.calls))
    Flow
      .decide(record)((s, kept) => stalling(s.at)(kept).flatMap(k => slotted(s, k.kinds)))
      .andThen(
        Flow
          .each[Env, Slotted, Stalled, Sending, (Place, Boolean)](sends)(sending)
          .keeping
          .map((s: Slotted, sent: Vector[(Place, Boolean)]) => calls(s, sent.toMap))
      )
      .andThen(
        Flow
          .each[Env, Calls, Stalled, Call, Unit](_.calls)(one)
          .keeping
          .map((c: Calls, _: Vector[Unit]) => c.at.copy(round = c.next))
      )
  }

  /** `s`'s calls at their slots, bound as `kinds` say. */
  private def slotted(s: Settle, kinds: Vector[Kind]): Either[Stalled, Slotted] =
    stalling(s.at)(
      s.calls
        .zip(kinds)
        .zipWithIndex
        .foldLeft[Either[TurnFailure, Vector[(Slot, Pending, Kind)]]](Right(Vector.empty)) {
          case (acc, ((pending, kind), index)) =>
            for {
              done <- acc
              slot <- Slot.of(s.at.looping.turn, s.at.round, index)
            } yield done :+ ((slot, pending, kind))
        }
        .map(Slotted(s.at, s.next, _))
    )

  /** `s`'s free hosted calls, by the place each goes to, in the order each place is first
    * called; a call whose tool has no place is left out.
    */
  private def sends(s: Slotted): Vector[Sending] = {
    val free = s.calls.collect { case (slot, pending, Kind.Hosted(_, false, Some(place))) =>
      (place, (slot, pending))
    }
    free.map(_._1).distinct.map(place => Sending(s.at, place, free.filter(_._1 == place).map(_._2)))
  }

  /** `s`'s calls, each with whether its place's send reached a serving edge, as `sent` says. */
  private def calls(s: Slotted, sent: Map[Place, Boolean]): Calls =
    Calls(
      s.at,
      s.next,
      s.calls.map { (slot, pending, kind) =>
        val went = kind match {
          case Kind.Hosted(_, _, Some(place)) => sent.getOrElse(place, false)
          case _ => false
        }
        Call(s.at, slot, pending, kind, went)
      }
    )

  /** A send: `call:n:send` to the offer's workspace, else `call:n:send:{place}`. */
  private def sending: Flow[Env, Sending, Stalled, (Place, Boolean)] = {
    def sent(name: StepName[Sending]): Step[Env, Sending, Either[TurnFailure, Boolean]] =
      Step.transact[Env, Sending, Either[TurnFailure, Boolean]](
        name,
        s => Subject.Turn(s.at.looping.turn)
      )(r =>
        s =>
          (tx: Tx^) ?=> {
            val Looping(turn, _, pins, _, _, _, _) = s.at.looping
            val hosted = s.calls.foldLeft[Either[TurnFailure, Vector[(Slot, Bound.Hosted)]]](
              Right(Vector.empty)
            ) { case (acc, (slot, pending)) =>
              acc.flatMap(done => r.hosted(s.at, slot, pending).map(h => done :+ ((slot, h))))
            }
            hosted.flatMap(TurnHosted.dispatch(r.hosting, turn, s.place, _, Permit.Free, pins))
          }
      )
    val send = Flow.branch((s: Sending) =>
      if (s.at.looping.offer.workspace.contains(s.place)) Left(s) else Right(s)
    )(Flow.step(sent(Names.send)), Flow.step(sent(Names.sendTo)))
    send.decided((s, went) => stalling(s.at)(went.map(s.place -> _)))
  }

  /** One call settled: run or answered in its own step, asked about first when gated, or sent
    * to an edge when hosted.
    */
  private def one: Flow[Env, Call, Stalled, Unit] = {
    val here = Step[Env, Call, Either[TurnFailure, Settled]](Names.call)(r => c => r.settle(c))
    val decided = Step[Env, Decided, Either[TurnFailure, Settled]](Names.call)(r =>
      d => r.decide(d.call, d.approval)
    )
    val gated = approved.keeping
      .map((c, answer) => answer.map(Decided(c, _)))
      .andThen(Flow.step(decided).right[TurnFailure].map(_.flatten))
    val routed = Flow.branch((c: Call) => c.routing)(
      Flow.step(here),
      Flow.either(gated, TurnHosted.settle)
    )
    routed.decided((c, settled) => stalling(c.at)(settled.map(_ => ())))
  }

  /** `failed`'s failure as the loop's stall, where `at` says. */
  private def stalling[A](at: Rounds)(failed: Either[TurnFailure, A]): Either[Stalled, A] =
    failed.left.map(Stalled(_, at))
}
