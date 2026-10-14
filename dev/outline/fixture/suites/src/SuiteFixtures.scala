package grit.outline.fixture.suites

/** Fixtures the suites share, as a test source's object of helpers is. */
object SuiteFixtures {

  /** A seed, which FixtureUseTests names. */
  def seeded: Int = 1

  /** A label, which FixtureUseTests names qualified and also defines itself. */
  def label: String = "fixture"

  /** Never named by a suite. */
  def unnamed: Int = 0

  /** A helper class, which --body prints whole. */
  final class Box(val n: Int) {
    def doubled: Int = n * 2
  }
}
