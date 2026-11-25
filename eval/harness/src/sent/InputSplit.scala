package grit.eval.harness.sent

import grit.core.message.{Cost, Tokens, Usage}
import grit.core.model.Rates
import grit.core.provider.{ModelRequest, TokenEstimator}
import grit.turn.TurnRecord.WindowMessage

/** Where one call's input went, part by part ([[Breakdown]]), each part split by how its
  * provider counted it: `read` from the cache, `written` to it, or sent `uncached`.
  */
final case class InputSplit(read: Breakdown, written: Breakdown, uncached: Breakdown)
    extends caps.Pure {

  /** Every part, however it was counted. */
  def total: Breakdown = read + written + uncached

  /** What each part cost at `rates`, in USD, in [[Breakdown]]'s field order. */
  def cost(rates: Rates): Vector[BigDecimal] =
    read.parts.zip(written.parts).zip(uncached.parts).map { case ((r, w), u) =>
      (BigDecimal(Tokens.value(r)) * rates.read + BigDecimal(Tokens.value(w)) * rates.write +
        BigDecimal(Tokens.value(u)) * rates.input) / InputSplit.Million
    }

  /** Both, part by part. */
  def +(other: InputSplit): InputSplit =
    InputSplit(read + other.read, written + other.written, uncached + other.uncached)
}

object InputSplit {

  private val Million: BigDecimal = BigDecimal(1000000)

  val Zero: InputSplit = InputSplit(Breakdown.Zero, Breakdown.Zero, Breakdown.Zero)

  /** `request`, sent with `usage`, by part: its system prompt, its tools, then each message as
    * [[Breakdown.message]] places it, in that order, costed by `estimator` and scaled to
    * `usage.input`; of that order, the first `usage.cacheRead` tokens read from the cache,
    * the next `usage.cacheWrite` written to it, and the rest uncached, each summing to its
    * count exactly. `None` when the request estimates 0 or the input is 0.
    */
  def of(
      request: ModelRequest,
      window: Vector[WindowMessage],
      usage: Usage,
      estimator: TokenEstimator
  ): Option[InputSplit] = {
    val segments =
      Breakdown.Zero.copy(system = estimator.system(request.system)) +:
        Breakdown.Zero.copy(tools = estimator.tools(request.tools)) +:
        request.messages.zipWithIndex.map((m, i) => Breakdown.message(m, i, window, estimator))
    val whole = segments.map(s => Tokens.value(s.total)).sum
    val input = Tokens.value(usage.input)
    Option.when(whole > 0 && input > 0) {
      val read = math.min(Tokens.value(usage.cacheRead), input)
      val written = math.min(Tokens.value(usage.cacheWrite), input - read)
      // Where reading ends and writing ends, in estimated tokens.
      val readTo = BigDecimal(whole) * read / input
      val writtenTo = BigDecimal(whole) * (read + written) / input
      val starts = segments.scanLeft(0L)((at, s) => at + Tokens.value(s.total))
      def overlap(from: BigDecimal, to: BigDecimal, lo: BigDecimal, hi: BigDecimal): BigDecimal =
        (to.min(hi) - from.max(lo)).max(BigDecimal(0))
      val portions = segments.zip(starts).map { (s, at) =>
        val from = BigDecimal(at)
        val to = from + BigDecimal(Tokens.value(s.total))
        def part(lo: BigDecimal, hi: BigDecimal): Breakdown =
          if (s.total == Tokens.Zero) Breakdown.Zero
          else portion(s, overlap(from, to, lo, hi) / BigDecimal(Tokens.value(s.total)))
        (part(0, readTo), part(readTo, writtenTo), part(writtenTo, BigDecimal(whole)))
      }
      def sum(parts: Vector[Breakdown], to: Long): Breakdown =
        if (to == 0) Breakdown.Zero
        else parts.foldLeft(Breakdown.Zero)(_ + _).scaled(Tokens(to)).getOrElse(Breakdown.Zero)
      InputSplit(
        sum(portions.map(_._1), read),
        sum(portions.map(_._2), written),
        sum(portions.map(_._3), input - read - written)
      )
    }
  }

  /** `b`, each part times `share`, rounded to the nearest token. */
  private def portion(b: Breakdown, share: BigDecimal): Breakdown = {
    def of(t: Tokens): Tokens =
      Tokens(
        (BigDecimal(Tokens.value(t)) * share).setScale(0, BigDecimal.RoundingMode.HALF_UP).toLong
      )
    Breakdown(of(b.system), of(b.tools), of(b.asks), of(b.calls), of(b.results), of(b.recalled))
  }

  /** The parts' names, in [[Breakdown]]'s field order. */
  private val Labels: Vector[String] =
    Vector("system", "tools", "asks", "replies", "results", "window")

  /** `sent`'s calls whose ledger rows are kept, each its input by part ([[of]]) under
    * [[CharEstimate]]'s estimate, a line each, for a person reading a terminal: each part read
    * (`r`), written (`w`, where any was) and uncached (`u`); then the turn's tokens by part,
    * and what they cost at [[TurnRates.of]]'s rates beside its output's and the ledger's.
    */
  def lines(sent: TurnSent, estimator: TokenEstimator): Vector[String] = {
    val rated = TurnRates.of(sent)
    val split = sent.requests.flatMap((round, reply, request) =>
      sent
        .row(reply)
        .flatMap(r => of(request, sent.calls.window, r.usage, estimator).map(s => (round, r, s)))
    )
    def cell(s: InputSplit, i: Int): String = {
      def at(b: Breakdown): Long = b.parts.lift(i).fold(0L)(Tokens.value)
      val w = at(s.written)
      s"${at(s.read)}r${if (w > 0) s" ${w}w" else ""} ${at(s.uncached)}u"
    }
    def row(s: InputSplit): String =
      Labels.zipWithIndex.map((label, i) => s"$label ${cell(s, i)}").mkString(", ")
    val calls =
      split.map((round, r, s) => s"  call $round: ${Tokens.value(r.usage.input)} in; ${row(s)}")
    val turn = split.foldLeft(Zero)((sum, c) => sum + c._3)
    val costs = rated.map(p =>
      split.foldLeft(Vector.fill(Labels.size)(BigDecimal(0))) { case (sum, (_, r, s)) =>
        sum.zip(s.cost(p.pricing.at(r.usage.input))).map(_ + _)
      }
    )
    val output = split.map((_, r, _) => Tokens.value(r.usage.output)).sum
    val outputCost = rated.map(p =>
      split.map((_, r, _) => p.pricing.at(r.usage.input).outputPerToken * r.usage.output.toLong).sum
    )
    val recorded =
      split.foldLeft(Option(BigDecimal(0)))((sum, c) => sum.zip(c._2.usage.costUsd).map(_ + _))
    val costLine = (costs, outputCost) match {
      case (Some(parts), Some(out)) =>
        s"  cost: ${Labels.zip(parts).map((l, c) => s"$l ${Cost.cents(c)}").mkString(", ")}; " +
          s"output $output tokens ${Cost.cents(out)}; in all ${Cost.cents(parts.sum + out)}, " +
          s"the ledger's ${recorded.fold("unknown")(Cost.cents)}"
      case _ => "  cost: unpriced"
    }
    (s"input by part, read from the cache (r), written to it (w) and uncached (u), " +
      s"${TurnRates.described(rated)}:") +: (calls ++ Vector(s"  the turn: ${row(turn)}", costLine))
  }

  extension (t: Tokens) private def toLong: Long = Tokens.value(t)
}
