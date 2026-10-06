package grit.core.job

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

/** When a once slot a person asks for falls: at an instant, or a delay after now. */
enum When {
  case At(instant: Instant)
  case In(delay: FiniteDuration)

  /** Its instant, asked at `now`. */
  def from(now: Instant): Instant = this match {
    case At(instant) => instant
    case In(delay) => now.plusNanos(delay.toNanos)
  }
}
