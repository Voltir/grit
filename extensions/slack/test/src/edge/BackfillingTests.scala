package grit.slack.edge

import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
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
      val release = new CountDownLatch(1)
      val told = new AtomicBoolean(false)
      val entered = new CountDownLatch(1)
      val asked = new AtomicInteger
      val b = new Backfilling(
        Backfilling.Virtual,
        work(
          () => { asked.incrementAndGet(); true },
          stopping => {
            entered.countDown()
            while (!stopping()) Thread.onSpinWait()
            told.set(true)
            val _ = release.await(10, TimeUnit.SECONDS)
          }
        )
      )
      b.wake()
      entered.await(5, TimeUnit.SECONDS) ==> true
      val began = System.nanoTime()
      b.close(200.millis)
      val waited = (System.nanoTime() - began).nanos
      b.wake()
      release.countDown()
      (told.get, waited >= 200.millis, waited < 5.seconds, asked.get) ==> (true, true, true, 1)
    }
  }
}
