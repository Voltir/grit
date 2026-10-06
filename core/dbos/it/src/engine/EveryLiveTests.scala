package grit.dbos.engine

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*

import grit.dbos.sql.TestPostgres

import utest.*

/** [[Engine.every]]: an engine's passes, against a real Postgres (opening an engine takes its
  * database's lock).
  */
object EveryLiveTests extends TestSuite {

  private def eventually(done: => Boolean): Boolean = {
    val until = System.nanoTime() + 30.seconds.toNanos
    var held = done
    while (!held && System.nanoTime() < until) {
      Thread.sleep(20)
      held = done
    }
    held
  }

  val tests = Tests {
    test(
      "an engine's passes run one at a time, each period after the last, and none once it has closed"
    ) {
      val engine = LiveEngine.open(TestPostgres.freshDatabase("every"), "test")
      val (passes, under, most) = (new AtomicInteger(), new AtomicInteger(), new AtomicInteger())
      try {
        engine.every("grit.test-every", 10.millis) { () =>
          val now = under.incrementAndGet()
          most.accumulateAndGet(now, math.max)
          // Busy, not asleep, so the close's interrupt cannot cut a pass short.
          val until = System.nanoTime() + 30.millis.toNanos
          while (System.nanoTime() < until) {}
          under.decrementAndGet()
          val _ = passes.incrementAndGet()
        }
        assert(eventually(passes.get() >= 5))
      } finally engine.close()
      val closedAt = passes.get()
      Thread.sleep(300)
      engine.every("grit.test-every-late", 10.millis)(() => { val _ = passes.incrementAndGet() })
      Thread.sleep(100)
      (most.get(), under.get(), passes.get()) ==> (1, 0, closedAt)
    }
  }
}
