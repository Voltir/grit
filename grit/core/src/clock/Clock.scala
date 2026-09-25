package grit.core.clock

import java.time.Instant

/** Capability to read the time. */
trait Clock extends caps.SharedCapability {

  /** The wall-clock time now. */
  def now(): Instant

  /** A monotonic reading in milliseconds, for measuring an interval within one process; its
    * origin means nothing.
    */
  def millis(): Long
}

object Clock {

  /** The system's clock. */
  def system(): Clock^ = new Clock {
    def now(): Instant = Instant.now()
    def millis(): Long = System.nanoTime() / 1000000
  }
}
