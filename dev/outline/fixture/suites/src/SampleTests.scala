package grit.outline.fixture.suites

import grit.outline.fixture.{B, C, Limits}

/** A suite the outline tests read and nothing runs: its tests and its helper hold references to the fixture's
  * definitions, at lines no other change moves.
  */
object SampleTests {
  private def test(name: String)(body: => Any): Unit = ()

  /** A helper holding a reference to [[C]]. */
  def made: C = C(1)

  test("a B holds its C") {
    B(made, "note")
  }

  test("limits are built through of, with two limits or three") {
    (Limits.of(1, 2), Limits.of(1, 2, 3))
  }
}
