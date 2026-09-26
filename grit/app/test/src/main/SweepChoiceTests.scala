package grit.app.main

import scala.concurrent.duration.DurationInt

import utest.*

/** How often the engine sweeps (`GRIT_SWEEP`). */
object SweepChoiceTests extends TestSuite {

  val tests = Tests {
    test("GRIT_SWEEP: unset is the default; a duration of at least a second, or an error") {
      Main.sweepEvery(Map.empty) ==> Right(Main.DefaultSweep)
      Main.sweepEvery(Map("GRIT_SWEEP" -> "5s")) ==> Right(5.seconds)
      Main.sweepEvery(Map("GRIT_SWEEP" -> "1s")) ==> Right(1.second)
      Main.sweepEvery(Map("GRIT_SWEEP" -> "0s")) ==> Left("GRIT_SWEEP must be at least a second")
      Main.sweepEvery(Map("GRIT_SWEEP" -> "5")) ==>
        Left("GRIT_SWEEP: not a duration: write a whole number and s, m, h or d, as 30s or 3m")
    }
  }
}
