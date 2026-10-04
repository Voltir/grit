package grit.eval.harness.stats

import java.math.{MathContext, RoundingMode}

/** A USD amount as a report shows a price: in millidollars, thousandths of a dollar, written
  * `m$`. For display alone: an amount is kept and summed in exact USD.
  */
object Mills {

  /** `usd` in millidollars to two significant digits, but never fewer than its whole
    * millidollars, trailing zeros dropped, a half rounded away from zero: `m$2.9` for
    * $0.0029, `m$0.03` for $0.00003, `m$12` for $0.0124, `m$126` for $0.1256, `m$1`, `m$0`;
    * below 0 with its minus sign first, `-m$2.9`.
    */
  def figure(usd: BigDecimal): String = {
    val mills = (usd * 1000).bigDecimal
    val rounded =
      if (mills.abs.compareTo(java.math.BigDecimal.TEN) >= 0)
        mills.setScale(0, RoundingMode.HALF_UP)
      else mills.round(new MathContext(2, RoundingMode.HALF_UP))
    if (rounded.signum == 0) "m$0"
    else if (rounded.signum < 0) s"-m$$${rounded.negate.stripTrailingZeros.toPlainString}"
    else s"m$$${rounded.stripTrailingZeros.toPlainString}"
  }
}
