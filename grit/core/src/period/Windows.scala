package grit.core.period

import scala.concurrent.duration.*

/** How long a period may stay quiet (`idle`), how long it stays open once someone says it
  * is done (`grace`), and how long a closed period's raw entries are kept (`retention`).
  */
final case class Windows private (
    idle: FiniteDuration,
    grace: FiniteDuration,
    retention: FiniteDuration
)

object Windows {

  /** The windows, or why not: each must be positive, and `grace` no longer than `idle`. */
  def of(
      idle: FiniteDuration,
      grace: FiniteDuration,
      retention: FiniteDuration
  ): Either[String, Windows] = {
    def positive(name: String, d: FiniteDuration) =
      Either.cond(d > Duration.Zero, (), s"$name must be positive")
    for {
      _ <- positive("idle", idle)
      _ <- positive("grace", grace)
      _ <- positive("retention", retention)
      _ <- Either.cond(grace <= idle, (), "grace must be no longer than idle")
    } yield new Windows(idle, grace, retention)
  }

  /** 24 hours idle, 15 minutes' grace, 30 days' retention. */
  val Default: Windows = new Windows(24.hours, 15.minutes, 30.days)
}
