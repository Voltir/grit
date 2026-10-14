package grit.outline.fixture.suites

import grit.outline.fixture.Limits

/** A second suite the outline tests read and nothing runs, holding one reference to [[Limits]]. */
object MoreTests {
  private def test(name: String)(body: => Any): Unit = ()

  test("limits of nothing") {
    Limits.of(0, 0)
  }
}
