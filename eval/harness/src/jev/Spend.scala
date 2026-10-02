package grit.eval.harness.jev

import java.nio.charset.StandardCharsets

import grit.models.JevConfig

/** What asking Jev costs before it is asked. */
object Spend {

  /** The fewest bytes of a request the estimate counts as one token. Meant to over-estimate:
    * JSON text runs nearer four bytes to a token.
    */
  val BytesPerToken = 3

  /** What sending `request`, Jev's wire body, costs at most in USD: its UTF-8 bytes over
    * [[BytesPerToken]], rounded up, at [[JevConfig.UsdPerMillionInput]]; output is not billed.
    */
  def estimate(request: String): BigDecimal = {
    val bytes = request.getBytes(StandardCharsets.UTF_8).length
    val tokens = (bytes + BytesPerToken - 1) / BytesPerToken
    BigDecimal(tokens) * JevConfig.UsdPerMillionInput / BigDecimal(1_000_000)
  }
}

/** What a run has `spent` in USD, under its `cap`. */
final case class Budget private (cap: BigDecimal, spent: BigDecimal) {

  /** Whether a call estimated at `estimate` may be made: `false` when it would take what is
    * spent past the cap.
    */
  def allows(estimate: BigDecimal): Boolean = spent + estimate <= cap

  /** `usd` more spent. What a call costs can exceed its estimate, so `spent` may pass `cap`
    * by one call's estimating error.
    */
  def spend(usd: BigDecimal): Budget = copy(spent = spent + usd)
}

object Budget {

  /** A run's budget under `cap`, nothing spent, when its calls not answered by the cache are
    * `estimated` at no more than `cap` in all; else `Refused`, before any call.
    */
  def of(cap: BigDecimal, estimated: BigDecimal): Either[Refused, Budget] =
    Either.cond(estimated <= cap, new Budget(cap, BigDecimal(0)), Refused(estimated, cap))

  /** A run estimated over its cap. */
  final case class Refused(estimated: BigDecimal, cap: BigDecimal)
}
