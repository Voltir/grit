package grit.eval.harness.sent

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Cost, Message, Tokens}
import grit.core.model.{Pricing, Rates}
import grit.core.provider.{ModelRequest, TokenEstimator}
import grit.core.tool.ToolName

/** Stubbing results, simulated over a recorded turn: before each call, the turn's own old,
  * unreferenced results of read-only tools replaced by a one-line stub saying the call can be
  * repeated. Hindsight, not a rerun: every call after a stubbing is the one the turn recorded,
  * its output written without the stub in its way.
  */
object Stubbing {

  /** Stub a result whose round is older than the last `keep` (a round is a reply and the
    * results of its calls) and whose estimate is `floor` tokens or more.
    */
  final case class Rule private (keep: Int, floor: Tokens) extends caps.Pure

  object Rule {

    /** `Left` when `keep` or `floor` is below 0. */
    def of(keep: Int, floor: Tokens): Either[String, Rule] =
      if (keep < 0) Left(s"keep is below 0: $keep")
      else if (Tokens.value(floor) < 0) Left(s"floor is below 0: ${Tokens.value(floor)}")
      else Right(Rule(keep, floor))

    val Default: Rule = Rule(3, Tokens(2000))
  }

  /** Why the rule keeps a result whole; of several, the first in this order. */
  enum Kept extends caps.Pure {

    /** Its tool is not read-only ([[grit.core.tool.ToolSet.Entry.readOnly]]), or the turn's
      * tool set does not hold it.
      */
    case Writes

    /** Its round is one of the last [[Rule.keep]]. */
    case Recent

    /** It is under [[Rule.floor]]. */
    case Small

    /** The model's output since points at a line of it ([[Referenced]]). A name counts only
      * where it is new, so of a file read twice, output pointing at it credits the first
      * copy alone, and the second is never pointed at.
      */
    case Pointed
  }

  /** A tool result of the turn's own, answering a call made in `round` (from 0), and why the
    * rule keeps it, `None` when it stubs it.
    */
  final case class Judged(result: Referenced.Result, round: Int, kept: Option[Kept])
      extends caps.Pure

  /** Each tool result of `request` from message `own` on (those before are its window's), as
    * `rule` judges it just before `request` is sent, by `estimator`; `readOnly` holds the names
    * of the turn's read-only tools.
    */
  def judge(
      rule: Rule,
      request: ModelRequest,
      own: Int,
      readOnly: Set[String],
      estimator: TokenEstimator
  ): Vector[Judged] = {
    val rounds = roundsOf(request.messages, own)
    val now = rounds.lastOption.getOrElse(0)
    Referenced.results(request, None, estimator).filter(_.at >= own).map { r =>
      val round = rounds.lift(r.at).getOrElse(0) - 1
      val kept =
        if (!readOnly.contains(r.tool)) Some(Kept.Writes)
        else if (now - round <= rule.keep) Some(Kept.Recent)
        else if (Tokens.value(r.cost) < Tokens.value(rule.floor)) Some(Kept.Small)
        else if (r.pointed > 0) Some(Kept.Pointed)
        else None
      Judged(r, round, kept)
    }
  }

  /** One recorded call: its `request`, the `input` its provider counted, of it the prefix read
    * from the cache (`cacheRead`), and what it cost in USD, `None` when unpriced.
    */
  final case class Sent(
      request: ModelRequest,
      input: Tokens,
      cacheRead: Tokens,
      cost: Option[BigDecimal]
  ) extends caps.Pure

  /** A recorded turn as [[replay]] reads it: its `calls`, oldest first, each request appending
    * to the one before; its `answer`; the message its own begin at (`own`), after its window's;
    * the names of its `readOnly` tools; the line each call was `shown` as; the input `limit` it
    * ran under; and the `pricing` of its calls' pin.
    */
  final case class Recorded(
      calls: Vector[Sent],
      answer: Option[Message.Assistant],
      own: Int,
      readOnly: Set[String],
      shown: Map[ToolCallId, String],
      limit: Option[Tokens],
      pricing: Option[Pricing]
  ) extends caps.Pure

  object Recorded {

    /** `sent` as [[replay]] reads it: each of its calls whose ledger row is kept
      * ([[TurnSent.requests]]), its own messages after its window's, priced as
      * [[TurnRates.of]] prices it.
      */
    def of(sent: TurnSent): Recorded =
      Recorded(
        sent.requests.flatMap((_, reply, request) =>
          sent
            .row(reply)
            .map(r => Sent(request, r.usage.input, r.usage.cacheRead, r.usage.costUsd))
        ),
        sent.answer.map(_.message),
        sent.calls.window.size,
        sent.tools.filter(_.readOnly).map(e => ToolName.value(e.name)).toSet,
        sent.shown,
        sent.limit,
        TurnRates.of(sent).map(_.pricing)
      )
  }

  /** What a million input tokens cost uncached or written to the cache (`write`), and read from
    * it (`read`).
    */
  final case class Price(write: BigDecimal, read: BigDecimal) extends caps.Pure {

    /** The share of a tail the dropped tokens must reach for a stubbing to pay back within
      * [[Horizon]] calls: (w − r) / (r · H); `None` when reads are free, so none pays back.
      */
    def payback: Option[BigDecimal] =
      Option.when(read > 0)((write - read) / (read * Horizon))
  }

  object Price {

    /** `rates`: `write` the dearer of its uncached and its cache-write rate. */
    def of(rates: Rates): Price = Price(rates.input.max(rates.write), rates.read)

    /** Anthropic's ratios to its uncached rate, for a turn whose pin is unpriced. */
    val AnthropicRatio: Price = Price(BigDecimal("1.25"), BigDecimal("0.1"))
  }

  /** The calls after which a stubbing pays back that the priced trigger asks. */
  val Horizon: Int = 4

  /** The fewest tokens the priced trigger stubs. */
  val PricedFloor: Tokens = Tokens(20000)

  /** Of the limit, the least the pressure trigger must free to stub rather than end the turn. */
  val Frees: BigDecimal = BigDecimal("0.25")

  /** When a stubbing is made. */
  enum Trigger extends caps.Pure {

    /** Before every call at which the rule stubs anything: the rule alone. */
    case Each

    /** Before a call whose input, less what is stubbed already, is at or over the turn's
      * limit, when what it stubs is [[Frees]] of the limit or more; never without a limit.
      */
    case Pressure

    /** Before a call at which what it stubs, D, is [[PricedFloor]] or more and D over the tail
      * T is the [[Price.payback]] or more.
      */
    case Priced
  }

  /** Call `round` (from 0) of a turn with nothing stubbed before it: its `input` as counted; `dropped`
    * (D), what the rule would stub then, each result less its stub; `tail` (T), the request
    * from the first of them on, stubs in place, sent again uncached; both in
    * the provider's tokens (the estimate times the call's input over its request's estimate);
    * `pressed`, when its input is at or over the limit, the share of the limit D is; and
    * whether the priced trigger fires.
    */
  final case class Call(
      round: Int,
      input: Tokens,
      dropped: Tokens,
      tail: Tokens,
      pressed: Option[BigDecimal],
      priced: Boolean
  ) extends caps.Pure

  /** The turn stubbed at each `trigger`: `stubbings` made; what they `rewrote` in the cache in
    * all, of each stubbing's T the part its call read from the cache (the rest it sent uncached
    * anyway), in the provider's tokens; `stubbed` results in all, `dropped` tokens of them less
    * their stubs, of `results`, all the turn's own results (both by the estimate); of those
    * stubbed, the `repeated`, which a later call repeats (the same tool, the same shown line),
    * and the `pointed`, at which later output points with no repeat made before it, the
    * stubbings hindsight regrets; the calls' `input` in all, less what was stubbed before each,
    * against `recordedInput`; and its `cost`, the recorded one less each call's stubbed tokens
    * at the read rate, plus what each stubbing rewrote at the write rate less the read rate,
    * against `recordedCost`, both `None` when a call is unpriced.
    */
  final case class Outcome(
      trigger: Trigger,
      stubbings: Int,
      rewrote: Tokens,
      stubbed: Int,
      dropped: Tokens,
      results: Tokens,
      repeated: Int,
      pointed: Int,
      input: Tokens,
      recordedInput: Tokens,
      cost: Option[BigDecimal],
      recordedCost: Option[BigDecimal]
  ) extends caps.Pure

  /** `turn` under `rule`: each of its calls; its own results as the rule judges them before its
    * last call (`last`), nothing stubbed before; then its outcome under each trigger, in order.
    */
  final case class Replay(
      rule: Rule,
      calls: Vector[Call],
      last: Vector[Judged],
      outcomes: Vector[Outcome]
  ) extends caps.Pure

  /** `turn` replayed under `rule`, by `estimator`. */
  def replay(rule: Rule, turn: Recorded, estimator: TokenEstimator): Replay = {
    val calls = turn.calls.map { sent =>
      val at = step(rule, turn, sent, Set.empty, estimator)
      val pressed = turn.limit
        .filter(l => Tokens.value(sent.input) >= Tokens.value(l) && Tokens.value(l) > 0)
        .map(l => BigDecimal(at.dropped) / BigDecimal(Tokens.value(l)))
      Call(
        at.round,
        sent.input,
        Tokens(at.dropped),
        Tokens(at.tail),
        pressed,
        priced(at, price(turn, sent))
      )
    }
    Replay(
      rule,
      calls,
      turn.calls.lastOption.fold(Vector.empty[Judged])(c =>
        judge(rule, c.request, turn.own, turn.readOnly, estimator)
      ),
      Trigger.values.toVector.map(outcome(rule, turn, _, estimator))
    )
  }

  /** What the rule would stub before one call, beside what is stubbed already. */
  private final case class Step(
      round: Int,
      chosen: Vector[Judged],
      saving: Map[ToolCallId, Long],
      dropped: Long,
      tail: Long,
      factor: BigDecimal
  )

  private def step(
      rule: Rule,
      turn: Recorded,
      sent: Sent,
      already: Set[ToolCallId],
      estimator: TokenEstimator
  ): Step = {
    val messages = sent.request.messages
    val judged = judge(rule, sent.request, turn.own, turn.readOnly, estimator)
    val chosen = judged.filter(j => j.kept.isEmpty && !already.contains(j.result.call))
    val saving = judged.map { j =>
      val stub = Message.ToolResult(j.result.call, stubText(turn, j.result), false)
      j.result.call -> math.max(
        0L,
        Tokens.value(j.result.cost) - Tokens.value(estimator.message(stub))
      )
    }.toMap
    val estimate = Tokens.value(estimator.request(sent.request))
    val factor =
      if (estimate <= 0) BigDecimal(0)
      else BigDecimal(Tokens.value(sent.input)) / BigDecimal(estimate)
    def scaled(n: Long): Long =
      (BigDecimal(n) * factor).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
    val first = chosen.map(_.result.at).minOption
    val tail = first.fold(0L) { f =>
      val after = messages.drop(f).map(m => Tokens.value(estimator.message(m))).sum
      val stubbedAfter = judged
        .filter(j => j.result.at >= f && (already.contains(j.result.call) || chosen.contains(j)))
        .map(j => saving.getOrElse(j.result.call, 0L))
        .sum
      after - stubbedAfter
    }
    Step(
      roundsOf(messages, turn.own).lastOption.getOrElse(0),
      chosen,
      saving,
      scaled(chosen.map(j => saving.getOrElse(j.result.call, 0L)).sum),
      scaled(tail),
      factor
    )
  }

  private def priced(at: Step, price: Price): Boolean =
    at.dropped >= Tokens.value(PricedFloor) && at.tail > 0 &&
      price.payback.exists(BigDecimal(at.dropped) / BigDecimal(at.tail) >= _)

  private def price(turn: Recorded, sent: Sent): Price =
    turn.pricing.fold(Price.AnthropicRatio)(p => Price.of(p.at(sent.input)))

  private def outcome(
      rule: Rule,
      turn: Recorded,
      trigger: Trigger,
      estimator: TokenEstimator
  ): Outcome = {
    final case class State(
        stubbed: Map[ToolCallId, Int],
        saved: Map[ToolCallId, Long],
        stubbings: Int,
        rewritten: Long,
        input: Long,
        cost: Option[BigDecimal]
    )
    val start = State(Map.empty, Map.empty, 0, 0L, 0L, turn.pricing.map(_ => BigDecimal(0)))
    val end = turn.calls.foldLeft(start) { (s, sent) =>
      val at = step(rule, turn, sent, s.stubbed.keySet, estimator)
      def scaled(n: Long): Long =
        (BigDecimal(n) * at.factor).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
      val before = scaled(s.saved.values.sum)
      val fires = at.chosen.nonEmpty && (trigger match {
        case Trigger.Each => true
        case Trigger.Pressure =>
          turn.limit.exists(l =>
            Tokens.value(sent.input) - before >= Tokens.value(l) &&
              BigDecimal(at.dropped) >= Frees * BigDecimal(Tokens.value(l))
          )
        case Trigger.Priced => priced(at, price(turn, sent))
      })
      val stubbed =
        if (fires) s.stubbed ++ at.chosen.map(j => j.result.call -> at.round) else s.stubbed
      val saved =
        if (fires)
          s.saved ++ at.chosen.map(j => j.result.call -> at.saving.getOrElse(j.result.call, 0L))
        else s.saved
      val dropped = scaled(saved.values.sum)
      // Of the tail, what the call read from the cache: the rest, its newest messages, it sent
      // uncached anyway, and a call whose cache missed rewrites nothing more.
      val rewrite =
        if (!fires) 0L
        else
          math.max(
            0L,
            math.min(at.tail, Tokens.value(sent.cacheRead) + at.tail - Tokens.value(sent.input))
          )
      val rates = turn.pricing.map(p => Price.of(p.at(sent.input)))
      val cost = for {
        sum <- s.cost
        recorded <- sent.cost
        r <- rates
      } yield sum + recorded +
        (BigDecimal(rewrite) * (r.write - r.read) - BigDecimal(dropped) * r.read) / Million
      State(
        stubbed,
        saved,
        s.stubbings + (if (fires) 1 else 0),
        s.rewritten + rewrite,
        s.input + Tokens.value(sent.input) - dropped,
        cost
      )
    }
    val last = turn.calls.lastOption.map(_.request)
    val finalRead =
      last.fold(Vector.empty[Referenced.Result])(r => Referenced.results(r, turn.answer, estimator))
    val own = finalRead.filter(_.at >= turn.own)
    val lastRound = last.fold(0)(r => roundsOf(r.messages, turn.own).lastOption.getOrElse(0))
    // Each reply's round, and the results output up to it points at: a call's request holds
    // the replies before its round, and the answer is the reply to the last call.
    val pointedBy: Vector[(Int, Set[ToolCallId])] =
      turn.calls.map { c =>
        roundsOf(c.request.messages, turn.own).lastOption.getOrElse(0) - 1 ->
          Referenced.results(c.request, None, estimator).filter(_.pointed > 0).map(_.call).toSet
      } :+ (lastRound -> finalRead.filter(_.pointed > 0).map(_.call).toSet)
    val made = last.fold(Vector.empty[(ToolCallId, String, Int)])(r =>
      calls(r.messages, turn.own) ++ turn.answer.toVector.flatMap(a =>
        a.blocks.collect { case AssistantBlock.ToolCall(id, n, _) => (id, n, lastRound) }
      )
    )
    val toolOf = made.map((id, n, _) => id -> n).toMap
    def repeatedAt(id: ToolCallId, at: Int): Option[Int] =
      made.collect {
        case (other, n, round)
            if other != id && round >= at && toolOf.get(id).contains(n) &&
              turn.shown.get(id).exists(sh => turn.shown.get(other).contains(sh)) =>
          round
      }.minOption
    def pointedAt(id: ToolCallId, at: Int): Option[Int] =
      pointedBy.collectFirst { case (round, ids) if round >= at && ids.contains(id) => round }
    val repeated = end.stubbed.count((id, at) => repeatedAt(id, at).nonEmpty)
    val pointed = end.stubbed.count { (id, at) =>
      pointedAt(id, at).exists(p => !repeatedAt(id, at).exists(_ < p))
    }
    Outcome(
      trigger,
      end.stubbings,
      Tokens(end.rewritten),
      end.stubbed.size,
      Tokens(end.saved.values.sum),
      Tokens(own.map(r => Tokens.value(r.cost)).sum),
      repeated,
      pointed,
      Tokens(end.input),
      Tokens(turn.calls.map(c => Tokens.value(c.input)).sum),
      end.cost,
      turn.calls.foldLeft(Option(BigDecimal(0)))((sum, c) => sum.zip(c.cost).map(_ + _))
    )
  }

  /** `replay` as a person reads it in a terminal: a heading naming its rule, a line a call, then
    * a line a trigger.
    */
  def lines(replay: Replay): Vector[String] = {
    val rule = replay.rule
    val rounds = if (rule.keep == 1) "round" else "rounds"
    val head = s"stubbing results older than the last ${rule.keep} $rounds, " +
      s"of ${Tokens.value(rule.floor)} estimated tokens or more:"
    val calls = replay.calls.map { c =>
      val pressed = c.pressed.fold("")(p => s"; at the limit, stubbing frees ${percent(p)} of it")
      val priced = if (c.priced) "; the priced trigger fires" else ""
      s"  call ${c.round}: ${Tokens.value(c.input)} in; D ${Tokens.value(c.dropped)}, " +
        s"T ${Tokens.value(c.tail)}$pressed$priced"
    }
    val outcomes = replay.outcomes.map { o =>
      val stubbings = (if (o.stubbings == 1) "1 stubbing" else s"${o.stubbings} stubbings") +
        s" rewriting ${Tokens.value(o.rewrote)}"
      val results = if (o.stubbed == 1) "1 result" else s"${o.stubbed} results"
      val freed = share(Tokens.value(o.dropped), Tokens.value(o.results))
      val repeated = share(o.repeated.toLong, o.stubbed.toLong)
      val pointed = share(o.pointed.toLong, o.stubbed.toLong)
      val recorded = o.recordedCost.fold("unpriced")(Cost.cents)
      val simulated = o.cost.fold("unpriced")(Cost.cents)
      val cost = s"cost $recorded recorded, $simulated simulated"
      s"  ${triggered(o.trigger)}: $stubbings, $results, ${Tokens.value(o.dropped)} of " +
        s"${Tokens.value(o.results)} result tokens dropped ($freed); ${o.repeated} repeated " +
        s"($repeated), ${o.pointed} pointed at unrepeated ($pointed); input ${Tokens.value(o.recordedInput)} recorded, " +
        s"${Tokens.value(o.input)} simulated; $cost"
    }
    def of(kept: Option[Kept]): String = {
      val these = replay.last.filter(_.kept == kept)
      s"${these.size} (${these.map(j => Tokens.value(j.result.cost)).sum} tokens)"
    }
    val last = replay.calls.lastOption.fold("")(c => s" call ${c.round}") match {
      case at =>
        s"  before the last$at, nothing stubbed: ${of(None)} stubbable; kept " +
          s"${of(Some(Kept.Writes))} not read-only, ${of(Some(Kept.Recent))} recent, " +
          s"${of(Some(Kept.Small))} small, ${of(Some(Kept.Pointed))} pointed at"
    }
    ((head +: calls) :+ last) ++ outcomes
  }

  private def triggered(t: Trigger): String = t match {
    case Trigger.Each => "at every call"
    case Trigger.Pressure => "at the limit"
    case Trigger.Priced => "when priced"
  }

  private def share(part: Long, whole: Long): String =
    if (whole <= 0) "-" else percent(BigDecimal(part) / BigDecimal(whole))

  private def percent(p: BigDecimal): String =
    s"${(p * 100).setScale(1, BigDecimal.RoundingMode.HALF_UP)}%"

  private val Million: BigDecimal = BigDecimal(1000000)

  /** The stub a result would be replaced by: its call as shown, and that it was dropped. */
  private def stubText(turn: Recorded, r: Referenced.Result): String =
    s"${turn.shown.getOrElse(r.call, r.tool)}: grit dropped this result to keep the window small; make the call again to see it."

  /** For each message of `messages`, how many of the turn's own replies precede it or are it,
    * counting from `own`: a request's last is the round its call is.
    */
  private def roundsOf(messages: Vector[Message], own: Int): Vector[Int] =
    messages.zipWithIndex
      .scanLeft(0) { case (n, (m, i)) =>
        m match {
          case _: Message.Assistant if i >= own => n + 1
          case _ => n
        }
      }
      .drop(1)

  /** Every call the turn's own replies in `messages` made: its id, its tool and its round. */
  private def calls(messages: Vector[Message], own: Int): Vector[(ToolCallId, String, Int)] = {
    val rounds = roundsOf(messages, own)
    messages.zipWithIndex.flatMap {
      case (a: Message.Assistant, i) if i >= own =>
        a.blocks.collect { case AssistantBlock.ToolCall(id, n, _) =>
          (id, n, rounds.lift(i).getOrElse(1) - 1)
        }
      case _ => Vector.empty
    }
  }
}
