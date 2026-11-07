package grit.tui.runtime.loop

import java.util.concurrent.{CountDownLatch, TimeUnit}

import utest.*

object BackgroundTests extends TestSuite {

  val tests = Tests {

    test("a background that never parks leaves the virtual threads running") {
      // The terminal reader is such a body: a native read that returns every 100 ms, looped
      // without ever parking. Held on a virtual thread's carrier, as many of them as the
      // scheduler has carriers stop every virtual thread -- on one core, the engine opening
      // behind the screen.
      val carriers = java.lang.Runtime.getRuntime.availableProcessors
      val spinners = (1 to carriers).map { i =>
        Background.start(s"spin-$i") { () =>
          while (!Thread.currentThread().isInterrupted) { Thread.onSpinWait() }
        }
      }
      val ran = new CountDownLatch(1)
      try {
        val _ = Thread.ofVirtual().start(() => ran.countDown())
        assert(ran.await(2, TimeUnit.SECONDS))
      } finally { spinners.foreach(_.close()) }
    }
  }
}
