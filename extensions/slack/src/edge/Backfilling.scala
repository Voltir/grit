package grit.slack.edge

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scala.concurrent.duration.FiniteDuration

/** An edge's join backfills ([[SlackEdge.backfillJoins]]): `work`, run one at a time, each run
  * on what `start` starts it on, when it is wanted.
  */
private[slack] final class Backfilling(start: Backfilling.Start, work: Backfilling.Work^) {

  // Set once, by close, and read by the run under way between messages; and the run last
  // started, replaced only by wake. Both are written through their own atomic operations, and
  // wake is called from one thread (the kit's delivery round), so no two runs overlap.
  @caps.unsafe.untrackedCaptures
  private val stopping = new AtomicBoolean(false)

  @caps.unsafe.untrackedCaptures
  private val running = new AtomicReference[Option[Backfilling.Running]](None)

  /** Starts the work, told when to stop, unless a run is under way, the edge is closing, or
    * the work is not wanted.
    */
  def wake(): Unit =
    if (!stopping.get && !running.get.exists(_.alive) && work.wanted())
      running.set(Some(start(() => work.run(() => stopping.get))))

  /** Tells the run under way to stop, and waits for it to end at most `within`; nothing starts
    * after.
    */
  def close(within: FiniteDuration): Unit = {
    stopping.set(true)
    running.get.foreach(_.join(within))
  }
}

private[slack] object Backfilling {

  /** What is run. */
  trait Work {

    /** Whether there is work to run. */
    def wanted(): Boolean

    /** Runs it, asking `stopping` as it goes whether to stop. */
    def run(stopping: () => Boolean): Unit
  }

  /** Starts a body. */
  trait Start {
    def apply(body: () => Unit): Running
  }

  /** A body started. */
  trait Running {

    /** Whether it has not yet ended. */
    def alive: Boolean

    /** Waits for it to end, at most `within`. */
    def join(within: FiniteDuration): Unit
  }

  /** Each body on a virtual thread of its own. */
  object Virtual extends Start {
    def apply(body: () => Unit): Running = {
      val thread = Thread.ofVirtual().start(() => body())
      new Running {
        def alive: Boolean = thread.isAlive
        def join(within: FiniteDuration): Unit = {
          val _ = thread.join(java.time.Duration.ofNanos(within.toNanos))
        }
      }
    }
  }
}
