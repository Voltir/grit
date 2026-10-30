package grit.chat.settings

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.period.{LifecycleSettings, Probability, Windows}
import grit.core.place.Weight

/** The lifecycle's settings as its suites write them. */
object TestLifecycle {

  def p(x: Double): Probability =
    Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  def settings(
      idle: FiniteDuration,
      retention: FiniteDuration,
      balance: Int,
      settle: FiniteDuration,
      resolveAt: Double,
      asks: Int,
      weighted: Weight = Weight.Default
  ): LifecycleSettings =
    Windows
      .of(idle, retention, 180.days)
      .flatMap(LifecycleSettings.of(_, balance, settle, p(resolveAt), asks, weighted))
      .getOrElse(throw new java.lang.AssertionError("settings"))

  def weight(w: Double): Weight = Weight.of(w).fold(e => sys.error(e), identity)
}
