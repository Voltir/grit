package grit.core.period

/** A probability: between 0 and 1, both included. */
opaque type Probability = Double

object Probability {

  val Zero: Probability = 0.0

  val One: Probability = 1.0

  /** `p` as a probability; `None` outside [0, 1], or NaN. */
  def of(p: Double): Option[Probability] = Option.when(p >= 0.0 && p <= 1.0)(p)

  /** `p` clamped to [0, 1], NaN as 0. */
  def clamped(p: Double): Probability = if (p.isNaN) 0.0 else math.min(1.0, math.max(0.0, p))

  def value(p: Probability): Double = p

  extension (p: Probability) {
    def >=(other: Probability): Boolean = (p: Double) >= (other: Double)
    def >(other: Probability): Boolean = (p: Double) > (other: Double)
  }
}
