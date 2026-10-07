package grit.eval.harness.jev

import grit.eval.harness.capture.Digest

import utest.*

/** How a rebuilt question's state compares to its capture's. */
object DriftTests extends TestSuite {

  private val a = Digest.text("a")
  private val b = Digest.text("b")

  val tests = Tests {
    test("a state is the same, changed, no longer built, newly built, or neither") {
      Vector(
        Drift.of(Some(a), Some(a)),
        Drift.of(Some(a), Some(b)),
        Drift.of(Some(a), None),
        Drift.of(None, Some(b)),
        Drift.of(None, None)
      ) ==> Vector(Drift.Same, Drift.Changed, Drift.Unbuilt, Drift.Built, Drift.Neither)
    }
  }
}
