package grit.core.spend

/** The most one run may spend on model calls, in US dollars: a subagent's, set by its type. */
final case class SpendCap private (usd: BigDecimal) extends caps.Pure

object SpendCap {

  /** `usd` as a spend cap, or why not: not above zero. */
  def of(usd: BigDecimal): Either[String, SpendCap] =
    Either.cond(usd > 0, SpendCap(usd), s"a spend cap is a number of dollars above zero, not $usd")
}
