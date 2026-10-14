package grit.core.message

import java.math.{MathContext, RoundingMode}

/** What some model calls cost together, as far as their providers priced them. */
enum Cost {

  /** Every call was priced. */
  case Exact(usd: BigDecimal)

  /** Some call was not priced: `usd` is what the priced ones cost. */
  case AtLeast(usd: BigDecimal)

  /** Exact only when both are. */
  def +(other: Cost): Cost = (this, other) match {
    case (Exact(a), Exact(b)) => Exact(a + b)
    case (Exact(a), AtLeast(b)) => AtLeast(a + b)
    case (AtLeast(a), Exact(b)) => AtLeast(a + b)
    case (AtLeast(a), AtLeast(b)) => AtLeast(a + b)
  }

  /** As grit writes a cost: `$` and the dollars, trailing zeros dropped (`$0.00031`); `≥ `
    * first when some call was not priced.
    */
  def written: String = this match {
    case Exact(usd) => Cost.plain(usd)
    case AtLeast(usd) => s"≥ ${Cost.plain(usd)}"
  }

  /** As [[Cost.mills]] shows its `usd`; `≥ ` first when some call was not priced. */
  def inMills: String = this match {
    case Exact(usd) => Cost.mills(usd)
    case AtLeast(usd) => s"≥ ${Cost.mills(usd)}"
  }
}

object Cost {

  val Zero: Cost = Exact(BigDecimal(0))

  def of(usage: Usage): Cost = usage.costUsd.fold(AtLeast(BigDecimal(0)))(Exact(_))

  def total(usages: Iterable[Usage]): Cost = usages.foldLeft(Zero)(_ + of(_))

  /** `usd` in millidollars, thousandths of a dollar, written `m$`: two significant digits, but
    * never fewer than its whole millidollars, trailing zeros dropped, a half rounded away from
    * zero: `m$2.9` for $0.0029, `m$0.03` for $0.00003, `m$12` for $0.0124, `m$126` for $0.1256,
    * `m$0`; below 0 with its minus sign first, `-m$2.9`. For display alone: an amount is kept and
    * summed in exact USD.
    */
  def mills(usd: BigDecimal): String = {
    val mills = (usd * 1000).bigDecimal
    val rounded =
      if (mills.abs.compareTo(java.math.BigDecimal.TEN) >= 0)
        mills.setScale(0, RoundingMode.HALF_UP)
      else mills.round(new MathContext(2, RoundingMode.HALF_UP))
    if (rounded.signum == 0) "m$0"
    else if (rounded.signum < 0) s"-m$$${rounded.negate.stripTrailingZeros.toPlainString}"
    else s"m$$${rounded.stripTrailingZeros.toPlainString}"
  }

  private def plain(usd: BigDecimal): String =
    s"$$${usd.bigDecimal.stripTrailingZeros.toPlainString}"
}
