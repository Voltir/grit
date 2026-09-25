package grit.core.clock

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

/** Capability to read the time, and to wait. */
trait Clock extends caps.SharedCapability {

  /** The wall-clock time now. */
  def now(): Instant

  /** A monotonic reading in milliseconds, for measuring an interval within one process; its
    * origin means nothing.
    */
  def millis(): Long

  /** Returns after `duration` has passed; at once when it is not positive. */
  def sleep(duration: FiniteDuration): Unit
}

object Clock {

  /** The system's clock. */
  def system(): Clock^ = new Clock {
    def now(): Instant = Instant.now()
    def millis(): Long = System.nanoTime() / 1000000
    def sleep(duration: FiniteDuration): Unit =
      if (duration.toMillis > 0) Thread.sleep(duration.toMillis)
  }
}
