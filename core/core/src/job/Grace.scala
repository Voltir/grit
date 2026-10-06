package grit.core.job

import scala.concurrent.duration.FiniteDuration

/** How late a once slot may run after its instant before it is missed: zero or more. */
opaque type Grace = FiniteDuration

object Grace {

  /** `d` as a grace; `None` when it is negative. */
  def of(d: FiniteDuration): Option[Grace] = Option.when(d >= Zero)(d)

  val Zero: Grace = FiniteDuration(0, scala.concurrent.duration.SECONDS)

  def value(g: Grace): FiniteDuration = g
}
