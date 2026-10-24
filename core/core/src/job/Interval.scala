package grit.core.job

import scala.concurrent.duration.FiniteDuration

/** The length between an [[SlotRule.Every]] rule's slots: a minute or more. */
opaque type Interval = FiniteDuration

object Interval {

  /** `d` as an interval; `None` when it is under a minute. */
  def of(d: FiniteDuration): Option[Interval] = Option.when(d >= Minute)(d)

  val HalfHour: Interval = FiniteDuration(30, scala.concurrent.duration.MINUTES)

  private val Minute = FiniteDuration(1, scala.concurrent.duration.MINUTES)

  def value(i: Interval): FiniteDuration = i
}
