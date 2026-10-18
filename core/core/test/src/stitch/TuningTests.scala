package grit.core.stitch

import scala.concurrent.duration.*

import grit.core.period.Probability

import utest.*

/** [[Tuning.Default]]: what stitching ships with. */
object TuningTests extends TestSuite {

  val tests = Tests {
    test("the shipped tuning carries a burst prior at 0.3 over 120 seconds") {
      Tuning.Default.burst ==> Some(Tuning.Burst(Probability.clamped(0.3), 120.seconds))
    }
  }
}
