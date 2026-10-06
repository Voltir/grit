package grit.core.clock

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

/** A clock a test sets: `now` is `at` until the test moves it; a sleep moves it by its length. */
final class SetClock(start: Instant) extends Clock {

  // Moved only by the test that holds the clock, and by `sleep`, on one thread.
  @caps.unsafe.untrackedCaptures
  var at: Instant = start

  def now(): Instant = at
  def millis(): Long = at.toEpochMilli
  def sleep(duration: FiniteDuration): Unit =
    if (duration.toNanos > 0) at = at.plusNanos(duration.toNanos)
}
