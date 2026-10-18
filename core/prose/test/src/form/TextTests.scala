package grit.prose.form

import utest.*

/** Inline text put in its normal form by [[Text.of]]. */
object TextTests extends TestSuite {

  private final case class N(n: Int) extends caps.Pure

  val tests = Tests {

    test("Text.of merges adjacent spans that share both marks and notes") {
      val strong = Set[Mark](Mark.Strong)
      Text.of(
        Vector(
          Span("a", strong, Vector(N(1))),
          Span("b", strong, Vector(N(1))),
          Span("", Set.empty, Vector.empty),
          Span("c", strong, Vector(N(1))),
          Span("d", strong, Vector(N(2))),
          Span("e", Set.empty, Vector(N(2))),
          Span("f", Set.empty, Vector.empty),
          Span("g", Set.empty, Vector.empty)
        )
      ) ==> Text(
        Vector(
          Span("abc", strong, Vector(N(1))),
          Span("d", strong, Vector(N(2))),
          Span("e", Set.empty, Vector(N(2))),
          Span("fg", Set.empty, Vector.empty)
        )
      )
    }
  }
}
