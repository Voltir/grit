package grit.core.spend

import grit.core.message.Cost

/** The most one run may spend on model calls, in US dollars: a subagent's, set by its type. */
final case class SpendCap private (usd: BigDecimal) extends caps.Pure {

  /** Whether a run that has spent `spent` may ask again: only when every call was priced and
    * their sum is below this cap.
    */
  def admits(spent: Cost): Boolean = spent match {
    case Cost.Exact(usd) => usd < this.usd
    case Cost.AtLeast(_) => false
  }
}

object SpendCap {

  /** `usd` as a spend cap, or why not: not above zero. */
  def of(usd: BigDecimal): Either[String, SpendCap] =
    Either.cond(usd > 0, SpendCap(usd), s"a spend cap is a number of dollars above zero, not $usd")
}
