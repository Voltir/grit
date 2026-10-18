package grit.outline.fixture.suites

import grit.outline.fixture.typed.{Numbers, Shelf}

/** A suite naming the typed fixture's definitions only as types, and `Numbers` only in a helper. */
object ShelfTests {
  private def test(name: String)(body: => Any): Unit = ()

  /** A shelf for the tests. */
  def shelf: Shelf[Int] = new Numbers

  test("a shelf is held as a Shelf") {
    val s: Shelf[Int] = shelf
    s.hashCode
  }
}
