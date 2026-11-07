package grit.eval.harness.bench

import scala.concurrent.duration.DurationInt

import utest.*

/** A bench run's bounded launch ([[Watched.limited]]). */
object WatchedTests extends TestSuite {

  val tests = Tests {
    test("a limited run is TERMed at its limit in whole seconds and KILLed a minute later") {
      Watched.limited(10.minutes, "env", "A=1", "./mill").map(_.written) ==>
        Right("timeout -k 1m 600s env A=1 ./mill")
    }
  }
}
