package grit.tools

import grit.core.host.Sources
import grit.core.host.Sources.Read

import utest.*

/** [[SourceTools]] of Scala's sources: the line a read's answer begins with, and what it takes. */
object SourceToolsTests extends TestSuite {

  private val scala =
    SourceTools
      .of(Sources.Scala, "`show` for a definition", "`uses` for every use")
      .fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test(
      "the line names what answers a read, and what a search, after the lead the readout counts"
    ) {
      (scala.line(Read.Reading), scala.line(Read.Searching)) ==> (
        "Answered in fewer tokens by `show` for a definition.",
        "Answered in fewer tokens by `uses` for every use."
      )
    }

    test("source tools name what reads their sources and what searches them") {
      (
        SourceTools.of(Sources.Scala, " ", "s"),
        SourceTools.of(Sources.Scala, "r", "")
      ) ==> (
        Left("source tools name what reads their sources and what searches them"),
        Left("source tools name what reads their sources and what searches them")
      )
    }
  }
}
