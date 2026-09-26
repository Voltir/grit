package grit.core.period

import scala.concurrent.duration.*

/** How long a period may stay quiet before it lapses (`idle`), and how long a closed period's
  * raw entries are kept (`retention`).
  */
final case class Windows private (idle: FiniteDuration, retention: FiniteDuration)

object Windows {

  /** The windows, or why not: each must be positive. */
  def of(idle: FiniteDuration, retention: FiniteDuration): Either[String, Windows] = {
    def positive(name: String, d: FiniteDuration) =
      Either.cond(d > Duration.Zero, (), s"$name must be positive")
    for {
      _ <- positive("idle", idle)
      _ <- positive("retention", retention)
    } yield new Windows(idle, retention)
  }

  /** 24 hours idle, 30 days' retention. */
  val Default: Windows = new Windows(24.hours, 30.days)
}
