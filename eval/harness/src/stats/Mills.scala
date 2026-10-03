package grit.eval.harness.stats

import java.math.{MathContext, RoundingMode}

/** A USD amount as a report shows a price: in mills, thousandths of a dollar. For display
  * alone: an amount is kept and summed in exact USD.
  */
object Mills {

  /** `usd` in mills to two significant digits, but never fewer than its whole mills, trailing
    * zeros dropped, a half rounded away from zero: `1.9 mills` for $0.0019, `0.03 mills` for
    * $0.00003, `12 mills` for $0.0124, `126 mills` for $0.1256, `1 mill`, `0 mills`; below 0
    * with a minus sign.
    */
  def of(usd: BigDecimal): String = {
    val mills = (usd * 1000).bigDecimal
    val rounded =
      if (mills.abs.compareTo(java.math.BigDecimal.TEN) >= 0)
        mills.setScale(0, RoundingMode.HALF_UP)
      else mills.round(new MathContext(2, RoundingMode.HALF_UP))
    val written =
      if (rounded.signum == 0) "0" else rounded.stripTrailingZeros.toPlainString
    s"$written ${if (written == "1" || written == "-1") "mill" else "mills"}"
  }

  /** `usd` in mills ([[of]]), then exactly in dollars: `1.9 mills ($0.0019)`. */
  def withUsd(usd: BigDecimal): String = s"${of(usd)} ($$${usd.bigDecimal.toPlainString})"
}
