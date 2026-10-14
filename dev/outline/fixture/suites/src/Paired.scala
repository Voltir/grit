package grit.outline.fixture.suites

/** A class with a companion, both of which a suite names. */
final class Paired(val n: Int) {
  def twice: Int = n * 2
}

/** Paired's companion. */
object Paired {
  def one: Paired = new Paired(1)
}
