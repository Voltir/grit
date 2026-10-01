package grit.core.spend

import grit.core.message.Cost

/** Some recorded model calls together: how many, and what they cost as far as their providers
  * priced them.
  */
final case class Spend(calls: Int, cost: Cost) {

  /** Both together. */
  def +(other: Spend): Spend = Spend(calls + other.calls, cost + other.cost)
}

object Spend {

  /** No calls. */
  val Zero: Spend = Spend(0, Cost.Zero)
}
