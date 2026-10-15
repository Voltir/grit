package grit.turn

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.act.phase.Calling
import grit.core.approval.Approval
import grit.core.context.Window
import grit.core.durable.{Flow, Step, StepName}
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

/** A turn's tool loop, as a flow: each round's `gate:n` when the turn is gated, its model call,
  * and, for a reply that calls tools ([[TurnLoop.next]]), its `record-call:n`, its free hosted
  * calls sent to each place together (`dispatch:n` to the workspace, `reach:n:{place}` to a
  * service it reaches), and each call settled in order: in its `tool:n:j`, after `ask:n:j` and
  * a person's answer for one that asks, and after a hosted call's own steps
  * ([[TurnHosted.settle]]). Until a reply answers, or a step fails. A turn that reached its loop
  * before the patch [[Turn.Patches.Integrity]] shipped runs ungated. Its bodies act through an
  * [[Env]]; what crosses its rounds is [[Rounds]], on the wire.
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

    /** `record-call:n`'s body: `reply`, the reply to `at.round` that called tools, kept. */
    def record(at: Rounds, reply: Message.Assistant)(using Tx^): Either[TurnFailure, EntryId]

    /** What `pending`, a call of round `at.round`, is. Fails, as every member here does, when
      * the tools the round offers no longer make one toolbox: the turn's start checks that
      * first in every run ([[TurnFlow]]).
      */
    def kind(at: Rounds, pending: Pending): Either[TurnFailure, Kind]

    /** `pending`, the call at `slot` of round `at.round`, bound to its hosted tool. Fails, as
      * each member below that binds a call does, when it binds to no such tool: its kind was
      * read in an earlier run, and the call's tools changed since.
      */
    def hosted(at: Rounds, slot: Slot, pending: Pending): Either[TurnFailure, Bound.Hosted]

    /** What a person is shown of `call` when its tool asks them first. */
    def shown(call: Call): Either[TurnFailure, String]

    /** The `tool:n:j` body of `call`, refused or free: answered with its outcome, unrun, at
      * the turn's level, or run.
      */
    def settle(call: Call): Either[TurnFailure, Settled]

    /** The `tool:n:j` body of `call`, gated, answered by `approval`
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

  /** What a turn's loop runs under: `turn`, as `offer` offers it, its calls made as `pins`
    * says over `seen`, its window, as `budget` allows, their tools' schemas `strict` when the
    * turn's pair is known to enforce them, `asked` the classification its model is asked about
    * when the classifier was unsure; a call that asks a person first waits `answerWithin`.
    */
  final case class Looping(
      turn: TurnRef,
      offer: TurnOffer,
      pins: Turn.Picked,
      seen: Window,
      asked: Option[TurnTopics.Classification],
      budget: Budget,
      strict: Boolean,
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
      gating: Option[Map[Int, Gated]],
      replies: Vector[Message.Assistant]
  ) extends OfRound {

    /** Each round's gate so far, by its index: none for an ungated turn. */
    def gates: Map[Int, Gated] = gating.getOrElse(Map.empty)

    /** The turn's level: its first gate's, met with each later one's in turn;
      * [[Integrity.Authority]] before any.
      */
    def level: Integrity =
      gates.values.map(_.level).reduceOption(_.meet(_)).getOrElse(Integrity.Authority)

    /** What the turn's rows are believed at: its level, when it is gated. */
    def believed: Option[Integrity] = gating.map(_ => level)

    private[TurnRound] def gatedBy(gate: Gated): Rounds =
      copy(gating = gating.map(_.updated(round.index, gate)))
  }

  /** A loop that answered with `answer`, the reply to `at.round` with its calls dropped. */
  final case class Looped(answer: Message.Assistant, at: Rounds)

  /** A loop that ended for `failure`, where `at` says. */
  final case class Stalled(failure: TurnFailure, at: Rounds)

  /** The call `pending` of `at.round`'s reply, at `slot`, bound as `kind`; `sent`, whether its
    * round's free hosted calls to its place reached a serving edge.
    */
  final case class Call(at: Rounds, slot: Slot, pending: Pending, kind: Kind, sent: Boolean)
      extends OfCall {
    def id: ToolCallId = pending.call.id
  }

  /** `call`, of a hosted tool `tool` at `place`, which `asks` a person first or not. */
  final case class Hosting(call: Call, tool: ToolName, asks: Boolean, place: Option[Place])
      extends OfCall {
    def slot: Slot = call.slot
  }

  /** The names of the loop's steps, and of a person's wait: the one home of each. */
  object Names {
    val gate: StepName[OfRound] = StepName.keyed[OfRound]("gate")(numbered)
    val firstCall: StepName[Any] = StepName("call-model")
    val call: StepName[OfRound] = StepName.keyed[OfRound]("call-model")(numbered)
    val recordCall: StepName[OfRound] = StepName.keyed[OfRound]("record-call")(numbered)
    val dispatch: StepName[OfRound] = StepName.keyed[OfRound]("dispatch")(numbered)
    val reach: StepName[Sending] =
      StepName.keyed[Sending]("reach")(s => s"${s.round.index}:${s.place.written}")
    val tool: StepName[OfCall] = StepName.keyed[OfCall]("tool")(placed)
    val ask: StepName[OfCall] = StepName.keyed[OfCall]("ask")(placed)
    val dispatchOne: StepName[OfCall] = StepName.keyed[OfCall]("dispatch")(placed)
    val expire: StepName[OfCall] = StepName.keyed[OfCall]("expire")(placed)
    val abandon: StepName[OfCall] = StepName.keyed[OfCall]("abandon")(placed)

    /** A person's time to answer about a call: a wait's label, never recorded. */
    val answer: StepName[OfCall] = StepName.keyed[OfCall]("wait")(placed)

    private def numbered(r: OfRound): String = r.round.index.toString
    private def placed(c: OfCall): String = s"${c.slot.round.index}:${c.slot.index}"
  }

  /** The loop from its first round, run under its input: its hosted calls go to the places its
    * offer gives them, sent as its pins say.
    */
  def flow: Flow[Env, Looping, Stalled, Looped] = {
    def from(gated: Boolean): Flow[Env, Looping, Stalled, Looped] =
      Flow
        .pass[Looping]
        .map(l => Rounds(l, Round.First, Option.when(gated)(Map.empty), Vector.empty))
        .andThen(Flow.loop(round(gated)))
    Flow.patch(Turn.Patches.Integrity)(from(false), from(true))
  }

  /** What a row `turn` writes after its first gate is believed at: `believed`, its level then
    * ([[Rounds.believed]]), or, for a turn ungated ([[Turn.Patches.Integrity]]), what its
    * entries say ([[EntryStore.believed]]).
    */
  private[turn] def believedAt(entries: EntryStore, turn: TurnRef, believed: Option[Integrity])(
      using Tx^
  ): Either[TurnFailure, Integrity] =
    believed.fold(entries.believed(turn).left.map(TurnFailure.ofStore))(Right(_))

  /** A person's answer about `call`, waited for as long as its loop gives them
    * ([[Calling.approval]]).
    */
  private[turn] val approval: Flow[Any, Call, Nothing, Approval] =
    Calling.approval[Call](Names.answer, _.id, _.at.looping.answerWithin)

  /** `ask:n:j`: a person asked about the call, shown what its tool asks them. */
  private[turn] val asked: Step[Env, Call, Either[TurnFailure, EntryId]] = {
    Step.transact[Env, Call, Either[TurnFailure, EntryId]](
      Names.ask,
      c => Subject.Turn(c.slot.turn)
    )(r =>
      c =>
        (tx: Tx^) ?=>
          r.shown(c)
            .flatMap(TurnTools.ask(r.settling.entries, c.slot, c.id, _, r.now()))
    )
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
  private def round(gates: Boolean): Flow[Env, Rounds, Stalled, Either[Rounds, Looped]] = {
    val gate = Step[Env, Rounds, Either[TurnFailure, Gated]](Names.gate)(r => at => r.gate(at))
    val gating: Flow[Env, Rounds, Stalled, Rounds] =
      if (gates) Flow.decide(gate)((at, g) => g.map(at.gatedBy).left.map(Stalled(_, at)))
      else Flow.pass[Rounds]
    def called(name: StepName[OfRound]): Step[Env, Rounds, Either[TurnFailure, Message.Assistant]] =
      Step[Env, Rounds, Either[TurnFailure, Message.Assistant]](name)(r =>
        at => r.call(at, TurnLoop.use(at.looping.budget, at.round))
      )
    val calling: Flow[Env, Rounds, Nothing, Either[TurnFailure, Message.Assistant]] =
      Flow.branch((at: Rounds) => if (at.round.index == Round.First.index) Left(at) else Right(at))(
        Flow.step(called(Names.firstCall)),
        Flow.step(called(Names.call))
      )
    gating
      .andThen(Flow.mayEnd(calling.keeping.map(replied)))
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
            val said = s"the reply to ${Round.step(at.round)} said nothing and called no tool"
            Left(Stalled(TurnFailure.Model(said), now))
          case Next.Settle(calls, next) => Right(Left(Settle(now, reply, calls, next)))
        }
    }

  /** A reply's calls settled: the reply kept, its calls bound, its free hosted calls sent, and
    * each call settled in order; then the round after.
    */
  private def settles: Flow[Env, Settle, Stalled, Rounds] = {
    val record = Step.transact[Env, Settle, Either[TurnFailure, EntryId]](
      Names.recordCall,
      s => Subject.Turn(s.at.looping.turn)
    )(r => s => (tx: Tx^) ?=> r.record(s.at, s.reply))
    // The kinds are not recorded: every run binds the round's calls again, as it always has.
    val bound = Flow.unrecorded[Env, Settle, Either[TurnFailure, Vector[Kind]]](r =>
      s =>
        s.calls.foldLeft[Either[TurnFailure, Vector[Kind]]](Right(Vector.empty))((acc, pending) =>
          acc.flatMap(done => r.kind(s.at, pending).map(done :+ _))
        )
    )
    Flow
      .decide(record)((s, kept) => kept.map(_ => s).left.map(Stalled(_, s.at)))
      .andThen(
        Flow.mayEnd(
          bound.keeping.map((s: Settle, kinds: Either[TurnFailure, Vector[Kind]]) =>
            kinds.left.map(Stalled(_, s.at)).flatMap(slotted(s, _))
          )
        )
      )
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
      .left
      .map(Stalled(_, s.at))

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

  /** A send: `dispatch:n` to the offer's workspace, else `reach:n:{place}`. */
  private def sending: Flow[Env, Sending, Stalled, (Place, Boolean)] = {
    def sent(name: StepName[Sending]): Step[Env, Sending, Either[TurnFailure, Boolean]] =
      Step.transact[Env, Sending, Either[TurnFailure, Boolean]](
        name,
        s => Subject.Turn(s.at.looping.turn)
      )(r =>
        s =>
          (tx: Tx^) ?=> {
            val Looping(turn, _, pins, _, _, _, _, _) = s.at.looping
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
    )(Flow.step(sent(Names.dispatch)), Flow.step(sent(Names.reach)))
    Flow.mayEnd(send.keeping.map((s, went) => went.map(s.place -> _).left.map(Stalled(_, s.at))))
  }

  /** One call settled: run or answered in its own step, asked about first when gated, or sent
    * to an edge when hosted.
    */
  private def one: Flow[Env, Call, Stalled, Unit] = {
    val here = Step[Env, Call, Either[TurnFailure, Settled]](Names.tool)(r => c => r.settle(c))
    val decided = Step[Env, Decided, Either[TurnFailure, Settled]](Names.tool)(r =>
      d => r.decide(d.call, d.approval)
    )
    val approved: Flow[Env, Call, Nothing, Either[TurnFailure, Settled]] =
      Flow
        .step(asked)
        .keeping
        .map[Either[TurnFailure, Call]]((c, kept) => kept.map(_ => c))
        .andThen(
          approval.keeping
            .map(Decided(_, _))
            .andThen(Flow.step(decided))
            .right[TurnFailure]
            .map(_.flatten)
        )
    val routed = Flow.branch((c: Call) =>
      c.kind match {
        case Kind.Refused | Kind.Free => Left(c)
        case Kind.Gated => Right(Left(c))
        case Kind.Hosted(tool, asks, place) => Right(Right(Hosting(c, tool, asks, place)))
      }
    )(Flow.step(here), Flow.either(approved, TurnHosted.settle))
    Flow.mayEnd(routed.keeping.map((c, settled) => settled.map(_ => ()).left.map(Stalled(_, c.at))))
  }
}
