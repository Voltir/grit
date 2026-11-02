package grit.core.context

import utest.*

object SectionTagTests extends TestSuite {

  /** The tags SectionTag holds, and no more: a window's framing grows in Fenced.
    *
    * Why it freezes: a bracket tag opens a line and nothing closes it, so text a window shows
    * could begin a line with one and pass for grit's own. A Fenced tag closes, escapes its own
    * closing tag inside, and a material one is named in Fenced.Rule. A new case here is a
    * design change to argue for, not a list to update.
    */
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
