package grit.slack.edge

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.concurrent.duration.*

import utest.*

/** [[Backfilling]]: an edge's join backfills, run one at a time on a thread of their own and
  * stopped when the edge closes.
  */
object BackfillingTests extends TestSuite {

  /** Runs nothing: counts the bodies it is given, each alive until a test ends it. */
  private final class Held extends Backfilling.Start {
    @caps.unsafe.untrackedCaptures
    var started = 0

    @caps.unsafe.untrackedCaptures
    var ended = 0

    def apply(body: () => Unit): Backfilling.Running = {
      val i = started
      started += 1
      new Backfilling.Running {
        def alive: Boolean = ended <= i
        def join(within: FiniteDuration): Unit = ()
      }
    }
  }

  /** Work wanted as `wanted` says, run as `run` says. */
  private def work(isWanted: () -> Boolean, body: (() => Boolean) -> Unit): Backfilling.Work =
    new Backfilling.Work {
      def wanted(): Boolean = isWanted()
      def run(stopping: () => Boolean): Unit = body(stopping)
    }

  val tests = Tests {
    test(
      "work is started when wanted, one run at a time: a wake while one runs starts none, and asks nothing"
    ) {
      val start = new Held
      val asked = new AtomicInteger
      val b = new Backfilling(start, work(() => { asked.incrementAndGet(); true }, _ => ()))
      b.wake()
      b.wake()
      val whileRunning = (start.started, asked.get)
      start.ended = 1
      b.wake()
      (whileRunning, start.started, asked.get) ==> ((1, 1), 2, 2)
    }

    test("work not wanted is not started") {
      val start = new Held
      val b = new Backfilling(start, work(() => false, _ => ()))
      b.wake()
      start.started ==> 0
    }

    test(
      "close tells the work under way to stop and waits for it at most `within`; after close no work starts"
    ) {
      val starts = new AtomicInteger
      val first = new AtomicReference[Option[Backfilling.Running]](None)
      val counted: Backfilling.Start = new Backfilling.Start {
        def apply(body: () => Unit): Backfilling.Running = {
          val _ = starts.incrementAndGet()
          val running = Backfilling.Virtual(body)
          val _ = first.compareAndSet(None, Some(running))
          running
        }
      }
      val entered = new CountDownLatch(1)
      val told = new CountDownLatch(1)
      val release = new CountDownLatch(1)
      val asked = new AtomicInteger
      val b = new Backfilling(
        counted,
        work(
          () => { asked.incrementAndGet(); true },
          stopping => {
            entered.countDown()
            // Polls as a run does between messages, parked between looks; gives up after a
            // minute so a broken close fails the test rather than hanging it.
            val giveUp = System.nanoTime() + 60.seconds.toNanos
            while (!stopping() && System.nanoTime() < giveUp)
              java.util.concurrent.locks.LockSupport.parkNanos(1.millis.toNanos)
            if (stopping()) told.countDown()
            val _ = release.await(60, TimeUnit.SECONDS)
          }
        )
      )
      b.wake()
      val began = entered.await(30, TimeUnit.SECONDS)
      val closing = System.nanoTime()
      // The run is held until released, so close waits out all of `within` and returns.
      b.close(200.millis)
      val waited = (System.nanoTime() - closing).nanos
      val wasTold = told.await(30, TimeUnit.SECONDS)
      release.countDown()
      first.get.foreach(_.join(30.seconds))
      val runEnded = first.get.exists(!_.alive)
      // The run has ended, so only closing can keep a wake from starting another.
      b.wake()
      (began, wasTold, runEnded, waited >= 200.millis, waited < 30.seconds) ==>
        (true, true, true, true, true)
      (asked.get, starts.get) ==> (1, 1)
    }
  }
}
