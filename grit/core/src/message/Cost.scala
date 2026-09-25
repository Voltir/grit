package grit.core.message

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
}

object Cost {

  val Zero: Cost = Exact(BigDecimal(0))

  def of(usage: Usage): Cost = usage.costUsd.fold(AtLeast(BigDecimal(0)))(Exact(_))

  def total(usages: Iterable[Usage]): Cost = usages.foldLeft(Zero)(_ + of(_))
}
