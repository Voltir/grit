package grit.dbos.workflow

import scala.concurrent.duration.FiniteDuration

/** The workflow bodies running in this process, counted as [[DurableWorkflow]] runs them, so
  * the engine can wait for them before it releases its lock: DBOS's shutdown interrupts them
  * and does not wait.
  */
final class Running {

  // Guarded by this object's monitor.
  @caps.unsafe.untrackedCaptures
  private var count = 0

  private[workflow] def enter(): Unit = synchronized { count += 1 }

  private[workflow] def exit(): Unit = synchronized { count -= 1; notifyAll() }

  /** Waits up to `within` for no body to be running; whether none is. */
  def awaitNone(within: FiniteDuration): Boolean = synchronized {
    val until = System.nanoTime() + within.toNanos
    while (count > 0 && System.nanoTime() < until)
      wait(((until - System.nanoTime()) / 1000000).max(1))
    count == 0
  }
}
