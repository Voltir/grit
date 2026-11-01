package grit.core.context

import utest.*

object SectionTagTests extends TestSuite {

  /** The tags SectionTag holds, and no more: a window's framing grows in Fenced. */
  private val Frozen: Vector[String] = Vector("[record]", "[afar]", "[strand]", "[gap]", "[doc]")

  val tests: Tests = Tests {
    test("SectionTag is frozen: new framing goes in Fenced") {
      val tags = SectionTag.values.toVector.map(_.tag)
      if (tags != Frozen)
        throw new java.lang.AssertionError(
          "SectionTag is frozen: frame new text in grit.core.prompt.Fenced's Tag, not here " +
            s"(cases: ${tags.mkString(", ")})"
        )
    }
  }
}
