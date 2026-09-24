package grit.tui.runtime.loop

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, TimeUnit}

import grit.tui.runtime.app.TimerId

import utest.*

/** The scheduler is a capability with real threads, so these tests use real time. Delays
  * are chosen so the *ordering* is what is asserted, never a duration.
  */
object SchedulerTests extends TestSuite {

  private val tick = TimerId.of("tick")
  private val other = TimerId.of("other")

  /** Run `body` with a scheduler that is always closed afterwards. */
  private def withScheduler(body: Scheduler => Unit): Unit = {
    val s = Scheduler.create()
    try { body(s) }
    finally { s.close() }
  }

  val tests = Tests {

    test("a cancelled timer never runs at all") {
      // The distinction that matters: next door a scheduled tick could not be cancelled,
      // so the defence was a generation stamp that let it fire and then dropped it in
      // `update`. Here the work simply does not happen.
      withScheduler { s =>
        val runs = new AtomicInteger(0)
        s.after(tick, 300L)(() => { val _ = runs.incrementAndGet() })
        assert(s.isPending(tick))
        s.cancel(tick)
        assert(!s.isPending(tick))
        Thread.sleep(600L)
        assert(runs.get == 0)
      }
    }

    test("re-arming an id replaces the pending timer instead of forking a second chain") {
      // FINDINGS 5.4: leaving an autoscroll edge and coming back started a second chain
      // while the first was still sleeping, and every round trip doubled them. With a
      // named timer, ten re-arms leave exactly one pending and fire exactly once.
      withScheduler { s =>
        val runs = new AtomicInteger(0)
        var i = 0
        while (i < 10) {
          s.after(tick, 150L)(() => { val _ = runs.incrementAndGet() })
          i += 1
        }
        assert(s.pendingCount == 1)
        Thread.sleep(500L)
        assert(runs.get == 1)
      }
    }

    test("distinct ids are independent") {
      withScheduler { s =>
        val fired = new CountDownLatch(2)
        s.after(tick, 10L)(() => fired.countDown())
        s.after(other, 10L)(() => fired.countDown())
        assert(fired.await(5L, TimeUnit.SECONDS))
      }
    }

    test("cancelling an id that is not pending is a no-op") {
      withScheduler { s =>
        s.cancel(tick)
        assert(!s.isPending(tick))
        assert(s.pendingCount == 0)
      }
    }

    test("close cancels everything pending and is idempotent") {
      // Teardown ordering: the scheduler stops before the terminal is restored, or a
      // late timer paints escape sequences into a shell already back in cooked mode.
      // Close is called from cleanup, from a finally, and from a shutdown hook.
      val s = Scheduler.create()
      val runs = new AtomicInteger(0)
      s.after(tick, 200L)(() => { val _ = runs.incrementAndGet() })
      s.after(other, 200L)(() => { val _ = runs.incrementAndGet() })
      assert(s.pendingCount == 2)
      s.close()
      s.close()
      s.close()
      assert(s.pendingCount == 0)
      Thread.sleep(500L)
      assert(runs.get == 0)
    }

    test("arming after close does nothing, so teardown cannot be raced") {
      val s = Scheduler.create()
      s.close()
      val runs = new AtomicInteger(0)
      s.after(tick, 10L)(() => { val _ = runs.incrementAndGet() })
      assert(!s.isPending(tick))
      Thread.sleep(300L)
      assert(runs.get == 0)
    }

    test("a fired timer stops being pending") {
      withScheduler { s =>
        val fired = new CountDownLatch(1)
        s.after(tick, 10L)(() => fired.countDown())
        assert(fired.await(5L, TimeUnit.SECONDS))
        Thread.sleep(100L)
        assert(!s.isPending(tick))
        assert(s.pendingCount == 0)
      }
    }
  }
}
