package grit.eval.harness.jev

import java.nio.charset.StandardCharsets

import grit.models.JevConfig

/** What asking Jev costs before it is asked. */
object Spend {

  /** The fewest bytes of a request the estimate counts as one token. Meant to over-estimate:
    * a request's text is billed nearer four bytes to a token.
    */
  val BytesPerToken = 3

  /** The input tokens Jev bills a call for beyond its body's: its own template around the
    * request. Meant to over-estimate: a triage request with a near-empty message is billed
    * about 550 tokens, where its body's bytes over [[BytesPerToken]] are about 440.
    */
  val TokensPerCall = 300

  /** What sending `request`, Jev's wire body, costs at most in USD: [[tokens]] at
    * [[JevConfig.UsdPerMillionInput]]; output is not billed.
    */
  def estimate(request: String): BigDecimal =
    BigDecimal(tokens(request)) * JevConfig.UsdPerMillionInput / BigDecimal(1_000_000)

  /** The input tokens [[estimate]] prices `request` at: its UTF-8 bytes over
    * [[BytesPerToken]], rounded up, and [[TokensPerCall]].
    */
  def tokens(request: String): Long = {
    val bytes = request.getBytes(StandardCharsets.UTF_8).length.toLong
    (bytes + BytesPerToken - 1) / BytesPerToken + TokensPerCall
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
