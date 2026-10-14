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

  /** As [[Cost.cents]] shows its `usd`; `≥ ` first when some call was not priced. */
  def inCents: String = this match {
    case Exact(usd) => Cost.cents(usd)
    case AtLeast(usd) => s"≥ ${Cost.cents(usd)}"
  }
}

object Cost {

  val Zero: Cost = Exact(BigDecimal(0))

  def of(usage: Usage): Cost = usage.costUsd.fold(AtLeast(BigDecimal(0)))(Exact(_))

  def total(usages: Iterable[Usage]): Cost = usages.foldLeft(Zero)(_ + of(_))

  /** `usd` in cents, written `¢` after the figure: two significant digits, but never fewer than
    * its whole cents, trailing zeros dropped, a half rounded away from zero: `0.29¢` for $0.0029,
    * `0.003¢` for $0.00003, `1.2¢` for $0.0124, `13¢` for $0.1256, `126¢` for $1.256, `0¢`; below
    * 0 with its minus sign first, `-0.29¢`. For display alone: an amount is kept and summed in
    * exact USD.
    */
  def cents(usd: BigDecimal): String = {
    val cents = (usd * 100).bigDecimal
    val rounded =
      if (cents.abs.compareTo(java.math.BigDecimal.TEN) >= 0)
        cents.setScale(0, RoundingMode.HALF_UP)
      else cents.round(new MathContext(2, RoundingMode.HALF_UP))
    if (rounded.signum == 0) "0¢"
    else if (rounded.signum < 0) s"-${rounded.negate.stripTrailingZeros.toPlainString}¢"
    else s"${rounded.stripTrailingZeros.toPlainString}¢"
  }

  private def plain(usd: BigDecimal): String =
    s"$$${usd.bigDecimal.stripTrailingZeros.toPlainString}"
}
