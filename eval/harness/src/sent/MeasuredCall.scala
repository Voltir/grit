package grit.eval.harness.sent

import grit.core.id.EntryId
import grit.core.message.{Message, Tokens, Usage}
import grit.core.provider.{ModelRequest, TokenEstimator}
import grit.models.AnthropicRates
import grit.turn.TurnRecord.WindowMessage

/** One model call of a recorded turn, its request rebuilt as `request`, against its ledger row
  * (`row`, `None` when none is kept): what its cache misses cost over cache reads (`missed`,
  * [[MeasuredCall.missed]]); how much of what the call before left in the cache it did not
  * read (`resent`, [[MeasuredCall.resent]]); its fresh tokens (`fresh`); and its input by part
  * in the provider's tokens (`parts`), each `None` when it cannot be told.
  */
final case class MeasuredCall(
    reply: EntryId,
    request: ModelRequest,
    row: Option[Spent],
    missed: Option[BigDecimal],
    resent: Option[Tokens],
    fresh: Option[Tokens],
    parts: Option[Breakdown]
) extends caps.Pure

object MeasuredCall {

  /** What `usage`'s uncached and written input cost on `model` over the same tokens read from
    * the cache, in USD, at [[AnthropicRates.pricing]] (the same tier both ways); `None` for a
    * model it has no rates for.
    */
  def missed(model: String, usage: Usage): Option[BigDecimal] =
    AnthropicRates
      .pricing(model)
      .map(p =>
        p.cost(usage) - p.cost(usage.copy(cacheRead = usage.input, cacheWrite = Tokens.Zero))
      )

  /** Of what the call before left in the cache (`previous`'s reads and writes), how much the
    * call that used `usage` did not read: 0 when its prefix held.
    */
  def resent(previous: Usage, usage: Usage): Tokens = {
    val left = Tokens.value(previous.cacheRead) + Tokens.value(previous.cacheWrite)
    Tokens(math.max(0L, left - Tokens.value(usage.cacheRead)))
  }

  /** Each of `calls` in order, its reply's entry and its request, measured against its row in
    * `rows`, its window's messages as `window` names them, costed by `estimator`.
    *
    * A call *appends* when its request is the call before's, with that call's reply after it,
    * then more, under the same system prompt and tools, and both have rows. Its fresh tokens are
    * then its input less the call before's input and the reply as re-sent: the reply's output
    * count, or, for a reply that withholds its reasoning ([[Message.withholds]]), the reply's
    * estimate times this call's input over this request's estimate. The first call's fresh
    * tokens are its whole input; a call that does not append has none. Fresh tokens may be
    * below 0 where the provider counts a reply re-sent below its output count.
    *
    * Its parts: for a call that appends with fresh tokens of 0 or more, the call before's,
    * the reply re-sent added to its own calls, then the messages after the reply
    * ([[Breakdown.messages]]) scaled to its fresh tokens; for any other, its whole request
    * ([[Breakdown.of]]) scaled to its input. Either way they sum to its input.
    */
  def of(
      calls: Vector[(EntryId, ModelRequest)],
      rows: EntryId -> Option[Spent],
      window: Vector[WindowMessage],
      estimator: TokenEstimator
  ): Vector[MeasuredCall] =
    calls
      .foldLeft(Vector.empty[MeasuredCall]) { case (done, (reply, request)) =>
        val row = rows(reply)
        val whole = row.flatMap(r => Breakdown.of(request, window, estimator).scaled(r.usage.input))
        val measured = row.fold(MeasuredCall(reply, request, None, None, None, None, None)) { r =>
          val before = done.lastOption.flatMap(b => b.row.map(br => (b, br)))
          val appended = before.flatMap((b, br) => appends(b.request, request).map(m => (b, br, m)))
          val (fresh, parts) = appended match {
            case None if done.isEmpty => (Some(r.usage.input), whole)
            case None => (None, whole)
            case Some((b, br, sentBack)) =>
              val carried =
                if (sentBack.withholds)
                  Tokens(
                    (BigDecimal(Tokens.value(estimator.message(sentBack))) *
                      Tokens.value(r.usage.input) / Tokens.value(estimator.request(request)))
                      .setScale(0, BigDecimal.RoundingMode.HALF_UP)
                      .toLong
                  )
                else br.usage.output
              val fresh = Tokens(
                Tokens.value(r.usage.input) - Tokens.value(br.usage.input) - Tokens.value(carried)
              )
              val added =
                if (Tokens.value(fresh) == 0) Some(Breakdown.Zero)
                else
                  Breakdown
                    .messages(request, b.request.messages.size + 1, window, estimator)
                    .scaled(fresh)
              val parts = (b.parts, added) match {
                case (Some(p), Some(a)) => Some(p + Breakdown.Zero.copy(calls = carried) + a)
                case _ => whole
              }
              (Some(fresh), parts)
          }
          MeasuredCall(
            reply,
            request,
            Some(r),
            missed(r.model, r.usage),
            before.map((_, br) => resent(br.usage, r.usage)),
            fresh,
            parts
          )
        }
        done :+ measured
      }

  /** The reply `request` repeats after `previous`'s messages, when it appends to `previous`. */
  private def appends(previous: ModelRequest, request: ModelRequest): Option[Message.Assistant] =
    if (request.system != previous.system || request.tools != previous.tools) None
    else if (!request.messages.startsWith(previous.messages)) None
    else
      request.messages.lift(previous.messages.size).collect { case a: Message.Assistant => a }
}
