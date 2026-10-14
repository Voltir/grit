package grit.outline.fixture.suites

import scala.util.Try

/** A suite whose helpers come partly from SuiteFixtures, imported in its body. */
object FixtureUseTests {
  import SuiteFixtures.*

  private def test(name: String)(body: => Any): Unit = ()

  /** The suite's own label, beside the fixtures'. */
  def label: String = "own"

  test("a seed, tried") {
    Try(seeded + Paired.one.twice)
  }

  test("a label is the fixture's") {
    SuiteFixtures.label
  }
}
