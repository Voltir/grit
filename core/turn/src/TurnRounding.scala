package grit.turn

import java.time.Instant

import grit.act.phase.Gated as CallGate
import grit.act.phase.{Asking, Calling, Shaped, Shaping}
import grit.core.act.{Extraction, Gates, Posed}
import grit.core.approval.Approval
import grit.core.context.Window
import grit.core.durable.StreamWriter
import grit.core.flow.{Extracted, Shape, Trusted}
import grit.core.id.{CallSlot, EntryId, EntrySeq, TurnRef}
import grit.core.message.Message
import grit.core.model.{Grade, StrictSchemas}
import grit.core.provider.{ModelRequest, ToolUse}
import grit.core.store.{Entry, Jot, Payload, Reads, StoreError, Tx}
import grit.core.tool.{Bound, Outcome, Repairs, ToolSet, Toolbox}
import grit.core.visibility.{Integrity, Subject}

import TurnLoop.{Pending, Round}
import TurnShown.{gatedOffer, hidSince, loopShape, shown, stubsAt}

/** What a turn's rounds act through ([[TurnRound.Env]]), over `env` and `tooling`: its model
  * told through `heard`, offered the tools its offer chose of `tooling`'s, their results kept
  * through its `jot`. What each round runs under is on the wire ([[TurnRound.Looping]]), so a
  * member finds the round's tools again from it, every time it is called.
  */
private[turn] final class Rounding[C^](
    heard: StreamWriter^,
    env: TurnEnv^,
    tooling: TurnTooling[C]^
) extends TurnRound.Env {

  def gate(at: TurnRound.Progress): Either[TurnFailure, Gated] =
    tools(at).flatMap { tools =>
      val l = at.looping
      // The name each item hidden so far was given, kept for as long as it stays hidden.
      val named = at.gates.values.flatMap(_.hidden.map((item, h) => item.ref -> h.name)).toMap
      val prior = Option.when(at.round.index != Round.First.index)(at.level)
      Rounding.gateOf(env, l.turn, l.seen, l.offer.root, tools.set, prior, named)(using l.routed)
    }

  def call(at: TurnRound.Progress, use: ToolUse): Either[TurnFailure, Message.Assistant] =
    for {
      offer <- offered(at)
      pins <- at.looping.routed.picked(env)
      tools <- Rounding.tools(tooling, offer, at.looping.asked)
      l = at.looping
      stubs = stubsAt(at.gates, at.round, tools.set)
      reply <- Rounding.callShaped(
        heard,
        l.turn,
        l.seen,
        stubs,
        Rounding.shapeAs(at, tools, use, pins)
      )(using env, pins, offer)
    } yield reply

  def record(at: TurnRound.Progress, reply: Message.Assistant, calls: Vector[Pending])(using
      Tx^
  ): Either[TurnFailure, TurnRound.Kept] =
    for {
      offer <- env.hosting.offered(at.looping.offer)
      pins <- at.looping.routed.pickedIn(env.records.profiles)
      tools <- Rounding.tools(tooling, offer, at.looping.asked)
      l = at.looping
      kinds = {
        val bind = binding(at, tools, pins)
        calls.map(p =>
          TurnTools.Kind.of(bind.bound(p, Extract.Unasked), tool => offer.placeOf(tool))
        )
      }
      (stubs, shape) <- Rounding.sent(tooling, at, offer, pins)
      direct <- TurnShown.direct(env.hosting.conversations, l.turn).left.map(TurnFailure.ofStore)
      kept <- Rounding.recordCall(
        offer,
        direct,
        env.records,
        l.turn,
        l.seen,
        stubs,
        at.round,
        reply,
        pins.turn.grade,
        shape,
        Some(at.level),
        env.clock.now()
      )
    } yield TurnRound.Kept(kept, kinds)

  def hosted(
      at: TurnRound.Progress,
      slot: TurnTools.Slot,
      pending: Pending
  ): Either[TurnFailure, Bound.Hosted] =
    rebinding(at).flatMap(_.rebind(slot, pending, Extract.Unasked) {
      case Right(hosted: Bound.Hosted) => Some(hosted)
      case _ => None
    })

  def asked(call: TurnRound.Call): Either[TurnFailure, CallGate.Ask] = {
    val gates = Gates.Asker(call.at.looping.answerWithin)
    rebinding(call.at).flatMap(_.rebind(call.slot, call.pending, Extract.Unasked) {
      case Right(gated: Bound.Gated) => asking(Calling.gate(gates, gated))
      case Right(hosted: Bound.Hosted) => asking(Calling.gate(gates, hosted))
      case _ => None
    })
  }

  private def asking(gated: CallGate): Option[CallGate.Ask] = gated match {
    case ask: CallGate.Ask => Some(ask)
    case CallGate.Free | CallGate.Refused(_) => None
  }

  // A call refused, or one that does not bind, is answered at the turn's level: grit's text
  // and what the call sent.
  def settle(
      call: TurnRound.Call,
      raise: Extraction.Raising^
  ): Either[TurnFailure, TurnTools.Settled] = {
    val (slot, id, level) = (call.slot, call.id, call.at.level)
    rebinding(call.at).flatMap(b =>
      b.rebind(slot, call.pending, b.extracting(raise)) {
        case Left(outcome) =>
          Some(settling.answer(slot, id, call.pending.call.name, outcome, Left(level), now()))
        case Right(free: Bound.Free) => Some(settling.run(slot, id, free, level, now()))
        case _ => None
      }.flatten
    )
  }

  def decide(call: TurnRound.Call, approval: Approval): Either[TurnFailure, TurnTools.Settled] =
    rebinding(call.at).flatMap(_.rebind(call.slot, call.pending, Extract.Unasked) {
      case Right(gated: Bound.Gated) =>
        Some(settling.decide(call.slot, call.id, gated, approval, call.at.level, now()))
      case _ => None
    }.flatten)

  def settling: TurnTools.Settling^{this} = new TurnTools.Settling(tooling.jot, env.records.entries)

  def now(): Instant = env.clock.now()

  def hosting: TurnHosting = env.hosting

  private def tools(at: TurnRound.Progress): Either[TurnFailure, Toolbox[C]] =
    offered(at).flatMap(Rounding.tools(tooling, _, at.looping.asked))

  // What round `at`'s offer names (TurnOffer.load says why a member may read it each time it
  // runs, unrecorded). Read as public, not bounded as the turn was routed: the rows are the
  // turn's own offer, kept by content id, and a clearance risen since must fail what the
  // turn reads of the store, not the offer it was already made.
  // Tool sets and prompt fragments are configuration, carrying no label, as profiles are.
  private def offered(at: TurnRound.Progress): Either[TurnFailure, TurnOffer] =
    env.db
      .read(Subject.Public)(
        TurnOffer.load(env.hosting.toolSets, env.hosting.prompts, at.looping.offer)
      )
      .left
      .map(TurnFailure.ofStore)

  // What round `at.round`'s calls bind against again inside a step's body, once per body.
  private def rebinding(at: TurnRound.Progress): Either[TurnFailure, Binding^{this}] =
    for {
      tools <- tools(at)
      pins <- at.looping.routed.picked(env)
    } yield binding(at, tools, pins)

  // What round `at.round`'s calls bind against, offered `tools` under `pins`, a tool that
  // reads the store reading as the turn's route bound it.
  private def binding(
      at: TurnRound.Progress,
      tools: Toolbox[C],
      pins: Turn.Picked
  ): Binding^{this} =
    new Binding(at, tools, pins, at.looping.routed.reads(env.db).as(Subject.Turn(at.looping.turn)))

  // A round's calls read against what round `at.round` offers: `tools`, after read_hidden and
  // extract over the items the round's gate hid when the round offers them (their names are
  // free: offered checked them), stamped at the turn's level, repaired as `pins`' turn pin
  // says, a tool that reads the store reading through `reads`. An ungated turn's stays at
  // Authority, so its calls bind to any tool whose requirement names no compartment, as every
  // turn's did before calls were stamped.
  private final class Binding(
      at: TurnRound.Progress,
      tools: Toolbox[C],
      pins: Turn.Picked,
      reads: Reads^
  ) {

    // `pending`, the call at `slot`, bound again, taken as `as` takes it. Its kind was
    // recorded with its round, in this run or one before it; a binding `as` does not take
    // means the call's tools changed between those runs, which a redeploy can do, since what
    // a call binds to is the deployment's, not the history's.
    def rebind[A, X^](slot: TurnTools.Slot, pending: Pending, answer: Extract.Answer^{X})(
        as: Either[
          Outcome,
          (Bound.Runs | Bound.Hosted)^{C, env, tooling, reads, X}
        ] ->{Rounding.this} Option[A]
    ): Either[TurnFailure, A] =
      as(bound[X](pending, answer)).toRight(
        TurnFailure.ToolsChanged(
          s"the call at round ${slot.round.index}, index ${slot.index}, binds otherwise than " +
            "its round recorded: its tools changed between runs"
        )
      )

    // `pending` bound to the round's tools, extract's questions put to `answer`.
    def bound[X^](
        pending: Pending,
        answer: Extract.Answer^{X}
    ): Either[Outcome, (Bound.Runs | Bound.Hosted)^{C, env, tooling, reads, X}] = {
      val box: Toolbox[caps.CapSet^{C, X}] =
        if (!hidSince(at.gates, at.round)) tools
        else {
          val g = at.gates.get(at.round.index)
          Toolbox
            .of[caps.CapSet^{X}](ReadHidden.tool(g), Extract.tool(g, answer))
            .flatMap(hidden => Toolbox.joined[caps.CapSet^{C, X}](hidden, tools))
            .getOrElse(tools)
        }
      val settings = pins.turn.settings
      TurnTools.read(box, pending, Repairs(settings.names, settings.repairs), reads, at.level)
    }

    // Extract's answers for a call this round settles, made through `raise`.
    def extracting(raise: Extraction.Raising^): Extract.Answer^{env, tooling, raise} = {
      val l = at.looping
      val level = Extract.level(at.gates.get(at.round.index))
      Rounding.extraction(env, tooling.jot, l.turn, l.seen, raise, level)(using pins)
    }
  }
}

private[turn] object Rounding {

  /** What round `at.round`'s request, offered as `offer` under `pins`, was sent: the stub of
    * each item its gate hid, by the item's ref, and how it is shaped from the plain request,
    * offering tools as its budget does. Fails as [[TurnRound.Env.record]] does.
    */
  def sent[C^](
      tooling: TurnTooling[C]^,
      at: TurnRound.Progress,
      offer: TurnOffer,
      pins: Turn.Picked
  ): Either[TurnFailure, (Map[ItemRef, String], ModelRequest -> ModelRequest)] =
    tools(tooling, offer, at.looping.asked).map(tools =>
      (
        stubsAt(at.gates, at.round, tools.set),
        shapeAs(at, tools, TurnLoop.use(at.looping.budget, at.round), pins)
      )
    )

  /** The tools a round offered as `offer` offers: `tooling`'s, as `offer` chose them, after
    * `topic` when its model is asked about `asked` ([[TurnShown.offered]]).
    */
  private def tools[C^](
      tooling: TurnTooling[C]^,
      offer: TurnOffer,
      asked: Option[TurnAgenda.Classification]
  ): Either[TurnFailure, Toolbox[C]] =
    TurnShown.offered(TurnOffer.toolbox(tooling, offer), asked)

  /** Round `at.round`'s request from the plain one, offering `tools` as `use` says, their
    * schemas strict when `pins`' turn pair is known to enforce them, and told and guided as
    * that pair is.
    */
  private def shapeAs[C^](
      at: TurnRound.Progress,
      tools: Toolbox[C],
      use: ToolUse,
      pins: Turn.Picked
  ): ModelRequest -> ModelRequest = {
    val settings = pins.turn.settings
    val strict = settings.strict == StrictSchemas.Enforced
    val (shown, lines) = gatedOffer(at.gates, at.round, tools.set, tools.schemas(strict), strict)
    loopShape(
      at.looping.asked,
      at.round,
      use,
      shown,
      settings.afterResult,
      settings.guidance,
      lines
    )
  }

  /** The `gate:n` step: what a request for `turn` over `window` shows, read through `env`'s
    * `db` as `pins` bound it ([[TurnShown.shown]]), each item as its row is believed
    * ([[TurnShown.itemsOf]]), decided by [[TurnGate.decide]] over `offered` from `prior`, else
    * from the turn's start, each hidden item keeping the name `named` gave it, and read as the
    * turn's replies called read_hidden ([[ReadHidden.read]]). The start is its root's
    * integrity, met with [[Integrity.Outside]] when `root` is a heard message grit was not
    * addressed in ([[TurnOffer.Root.Heard]], [[TurnOffer.Root.Named]]).
    */
  private def gateOf(
      env: TurnEnv^,
      turn: TurnRef,
      window: Window,
      root: TurnOffer.Root,
      offered: ToolSet,
      prior: Option[Integrity],
      named: Map[ItemRef, HiddenName]
  )(using pins: Turn.Bounded): Either[TurnFailure, Gated] =
    pins
      .reads(env.db)
      .read(Subject.Turn(turn))(shown(env.records, turn, window))
      .left
      .map(TurnFailure.ofStore)
      .map { showing =>
        val start = prior.getOrElse {
          val rooted =
            showing.mine
              .minByOption(e => EntrySeq.value(e.seq))
              .fold(Integrity.Untrusted)(_.integrity)
          root match {
            case TurnOffer.Root.Heard | TurnOffer.Root.Named => rooted.meet(Integrity.Outside)
            case TurnOffer.Root.Addressed | TurnOffer.Root.ByName => rooted
          }
        }
        TurnGate.decide(
          start,
          offered,
          TurnShown.itemsOf(showing, window),
          ReadHidden.read(showing.mine),
          named
        )
      }

  /** How extract puts a question of an item a request over `window` shows of `turn`
    * ([[Extract.Answer]]): to the summary's pin as `pins` cleared it, a quarantined JSON ask
    * ([[Extract.request]]) shown the item as the model would be shown it in full
    * ([[TurnShown.textOf]]), read through `env`'s `db` as `pins` bound it, and the question;
    * each of its model calls kept in the usage ledger through `jot` under [[Extract.costId]]
    * (one a rerun already kept is left as kept); its answer trusted through `raise` at `level`
    * ([[Extraction.Node]]). Why no answer: no summary pin cleared, the
    * item no longer kept, the store unreadable, the ask's provider failing or its reply not
    * reading after its repair.
    */
  private def extraction(
      env: TurnEnv^,
      jot: Jot^,
      turn: TurnRef,
      window: Window,
      raise: Extraction.Raising^,
      level: Integrity
  )(using
      pins: Turn.Picked
  ): (Item, String, Shape, CallSlot) ->{env, jot, raise} Either[String, Trusted[
    Option[Extracted]
  ]] =
    (item, question, shape, slot) =>
      for {
        pin <- pins.summary.left.map(_ => "no summary model is cleared for this turn")
        showing <- pins
          .reads(env.db)
          .read(Subject.Turn(turn))(shown(env.records, turn, window))
          .left
          .map(TurnFailure.describe)
        text <- TurnShown.textOf(showing, window, item.ref).toRight("the item is no longer kept")
        request <- Extract.request(text, question, shape, pin.settings.strict)
        typed <- Extraction.reply(shape)
        shaped = Shaping.shaped(
          env.models.provider(pin),
          request,
          Posed.ReplyTool,
          typed,
          pin.settings.names,
          pin.settings.repairs,
          Posed.Json.Repairs,
          env.clock
        )
        calls = shaped match {
          case Shaped.Read(_, _, calls) => calls
          case Shaped.Unread(_, calls) => calls
          case Shaped.Failed(_, calls) => calls
        }
        _ <- calls.zipWithIndex.foldLeft[Either[String, Unit]](Right(())) { case (acc, (c, k)) =>
          acc.flatMap { _ =>
            val (ledger, estimate) = (env.records.ledger, env.records.estimator.request(c.sent))
            jot.write(Subject.Turn(turn))(
              ledger.record(
                Extract.costId(slot, k),
                turn,
                turn.workflowId,
                c.answer.model,
                c.answer.upstream,
                Some(pin.grade),
                c.answer.usage,
                estimate
              )
            ) match {
              case Right(()) | Left(StoreError.DuplicateId(_)) => Right(())
              case Left(error) => Left(TurnFailure.describe(error))
            }
          }
        }
        answer <- shaped match {
          case Shaped.Read(_, value, _) => Right(value)
          case Shaped.Unread(why, _) => Left(why)
          case Shaped.Failed(why, _) => Left(why)
        }
      } yield raise((answer = answer, level = level))

  /** The `ask:n:record` step: `reply`, the reply to `round` that called tools, kept as
    * [[TurnTools.callId]] dated `at` after everything in the conversation, believed as
    * [[TurnRound.believedAt]] says of `believed`, and what it cost in the ledger beside the
    * estimate of its request, rebuilt by `shape` from the same window and entries under
    * `offer`'s system prompt, its message dated as `offer` and `direct` say
    * ([[TurnShown.requestOf]]), each item `stubs` names shown as its stub.
    */
  private def recordCall(
      offer: TurnOffer,
      direct: Boolean,
      records: TurnRecords,
      turn: TurnRef,
      window: Window,
      stubs: Map[ItemRef, String],
      round: Round,
      reply: Message.Assistant,
      grade: Grade,
      shape: ModelRequest -> ModelRequest,
      believed: Option[Integrity],
      at: Instant
  )(using Tx^): Either[TurnFailure, EntryId] = {
    val TurnRecords(entries, ledger, estimator, _, _, _, _) = records
    val id = TurnTools.callId(turn, round)
    for {
      next <- entries.lockNext(turn.conversationId).left.map(TurnFailure.ofStore)
      level <- TurnRound.believedAt(entries, turn, believed)
      showing <- shown(records, turn, window).left.map(TurnFailure.ofStore)
      sent <- TurnShown
        .requestOf(offer.system, showing, turn, window, stubs, offer.zoned, direct)
        .map(_.shaped(shape))
      _ <- entries
        .insert(
          Entry(
            id,
            turn.conversationId,
            turn.turnSeq,
            None,
            next.seq,
            Payload.Exchange(reply),
            at,
            level
          )
        )
        .left
        .map(TurnFailure.ofStore)
      _ <- Asking
        .spent(ledger, id, turn, reply, Some(grade), estimator.request(sent))
        .left
        .map(TurnFailure.ofStore)
    } yield id
  }

  /** The request built from `turn`'s `window`, each item `stubs` names shown as its stub, and
    * shaped by `shape`, sent to the provider, whose reply is told to edges as it arrives, asked
    * as [[Asking.reply]] says: each try a fresh attempt on the stream
    * ([[TurnStream.hearing]]), so a rerun's pieces follow a crashed run's; its failure fails
    * the call.
    */
  private def callShaped(
      heard: StreamWriter^,
      turn: TurnRef,
      window: Window,
      stubs: Map[ItemRef, String],
      shape: ModelRequest -> ModelRequest
  )(using
      env: TurnEnv^,
      pins: Turn.Picked,
      offer: TurnOffer
  ): Either[TurnFailure, Message.Assistant] =
    TurnShown.request(env, offer.system, turn, window, stubs, offer.zoned).flatMap { req =>
      val hearing = TurnStream.hearing(heard, env.fresh, env.clock)
      Asking
        .reply(env.models.provider(pins.turn), req.shaped(shape), hearing, env.clock)
        .left
        .map(TurnFailure.Model(_))
    }
}
