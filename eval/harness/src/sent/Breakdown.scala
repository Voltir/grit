package grit.eval.harness.sent

import grit.core.message.{Message, Tokens}
import grit.core.provider.{ModelRequest, TokenEstimator}
import grit.turn.TurnRecord.WindowMessage

/** Where a request's input goes, by part, in tokens: its system prompt; its tools'
  * descriptions; its briefs and asks (a window's brief, then each user message after the
  * window: the dated line, the person's message, grit's notes); its own calls (the model's
  * messages after the window); its tool results (after the window); and what its window
  * recalled (every other window message).
  */
final case class Breakdown(
    system: Tokens,
    tools: Tokens,
    asks: Tokens,
    calls: Tokens,
    results: Tokens,
    recalled: Tokens
) extends caps.Pure {

  /** Every part, in field order. */
  def parts: Vector[Tokens] = Vector(system, tools, asks, calls, results, recalled)

  /** Their sum. */
  def total: Tokens = parts.foldLeft(Tokens.Zero)(_ + _)

  /** Both, part by part. */
  def +(other: Breakdown): Breakdown =
    Breakdown(
      system + other.system,
      tools + other.tools,
      asks + other.asks,
      calls + other.calls,
      results + other.results,
      recalled + other.recalled
    )

  /** Each part times `to` over [[total]], summing to `to` exactly: each rounded down, then a
    * token more to the parts with the largest remainders, ties in field order. `None` when
    * [[total]] is 0 or `to` is below 0.
    */
  def scaled(to: Tokens): Option[Breakdown] = {
    val whole = Tokens.value(total)
    val target = Tokens.value(to)
    if (whole <= 0 || target < 0) None
    else {
      val shares = parts.map(p => BigInt(Tokens.value(p)) * target)
      val floors = shares.map(s => (s / whole).toLong)
      val left = (target - floors.sum).toInt
      // The parts the remainder goes to: the largest remainders, ties in field order.
      val more = shares.zipWithIndex
        .map((s, i) => (s % whole, i))
        .sortBy((r, i) => (-r, i))
        .take(left)
        .map(_._2)
        .toSet
      def share(part: Tokens, i: Int): Tokens = {
        val floor = (BigInt(Tokens.value(part)) * target / whole).toLong
        Tokens(if (more(i)) floor + 1 else floor)
      }
      Some(
        Breakdown(
          share(system, 0),
          share(tools, 1),
          share(asks, 2),
          share(calls, 3),
          share(results, 4),
          share(recalled, 5)
        )
      )
    }
  }
}

object Breakdown {

  /** Nothing in any part. */
  val Zero: Breakdown =
    Breakdown(Tokens.Zero, Tokens.Zero, Tokens.Zero, Tokens.Zero, Tokens.Zero, Tokens.Zero)

  /** `request` costed by `estimator`: its system prompt and tools, then each message as
    * [[messages]] places it. Sums to `estimator.request(request)`.
    */
  def of(
      request: ModelRequest,
      window: Vector[WindowMessage],
      estimator: TokenEstimator
  ): Breakdown =
    Zero.copy(system = estimator.system(request.system), tools = estimator.tools(request.tools)) +
      messages(request, 0, window, estimator)

  /** The messages of `request` from index `from` on, costed by `estimator`: one at an index
    * `window` covers by what it says, a brief to `asks` and the rest to `recalled`; any other
    * by its kind, a user message to `asks`, the model's to `calls`, a tool's result to
    * `results`.
    */
  def messages(
      request: ModelRequest,
      from: Int,
      window: Vector[WindowMessage],
      estimator: TokenEstimator
  ): Breakdown =
    request.messages.zipWithIndex.drop(from).foldLeft(Zero) { case (sum, (m, i)) =>
      val cost = estimator.message(m)
      sum + (window.lift(i) match {
        case Some(WindowMessage.Brief) => Zero.copy(asks = cost)
        case Some(WindowMessage.Recalled) => Zero.copy(recalled = cost)
        case None =>
          m match {
            case _: Message.User => Zero.copy(asks = cost)
            case _: Message.Assistant => Zero.copy(calls = cost)
            case _: Message.ToolResult => Zero.copy(results = cost)
          }
      })
    }
}
