package grit.tools

import grit.core.host.Argv
import grit.tools.SourceTools.Read

import utest.*

/** [[SourceTools]] of Scala's sources: which shell reads are reads of them, and what the rule
  * points at when no classifier answers.
  */
object SourceToolsTests extends TestSuite {

  private def argv(words: String*): Argv =
    Argv.of(words.head, words.tail*).fold(e => throw new java.lang.AssertionError(e), identity)

  private val scala =
    SourceTools
      .of("Scala", Vector(".scala"), "`show` for a definition", "`uses` for every use")
      .fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test(
      "a read names a source file, a search names one or recurses into its kind; no other is one"
    ) {
      Vector(
        argv("cat", "core/src/Turn.scala"),
        argv("head", "-n", "40", "a/B.scala"),
        argv("sed", "-n", "10,40p", "a/B.scala"),
        argv("grep", "-n", "Turn", "a/B.scala"),
        argv("grep", "-rn", "Turn", "core"),
        argv("grep", "-r", "--include=*.scala", "Turn", "."),
        argv("cat", "README.md"),
        argv("grep", "-rn", "--include=*.md", "Turn", "."),
        argv("grep", "Turn", "docs/a.md"),
        argv("ls", "a/B.scala"),
        argv("wc", "-l", "a/B.scala")
      ).map(scala.read) ==> Vector(
        Some(Read.Reading),
        Some(Read.Reading),
        Some(Read.Reading),
        Some(Read.Searching),
        Some(Read.Searching),
        Some(Read.Searching),
        None,
        None,
        None,
        None,
        None
      )
    }

    test(
      "the rule points at every read of a file, and at a search for an identifier that names no file of another kind"
    ) {
      Vector(
        argv("cat", "a/B.scala") -> Read.Reading,
        argv("grep", "-rn", "Turn", "core") -> Read.Searching,
        argv("grep", "-rn", "class Turn", "core") -> Read.Searching,
        argv("grep", "-rn", "\\bTurnEnv.coding\\b", "core") -> Read.Searching,
        argv("grep", "-rn", "is not on this", "core") -> Read.Searching,
        argv("grep", "-rn", "Turn\\(", "core") -> Read.Searching,
        argv("grep", "-rn", "Turn", "a/B.scala", "build.mill") -> Read.Searching
      ).map((a, r) => scala.ruled(a, r)) ==> Vector(true, true, true, true, false, false, false)
    }

    test(
      "the line names what answers a read, and what a search, after the lead the readout counts"
    ) {
      (scala.line(Read.Reading), scala.line(Read.Searching)) ==> (
        "Answered in fewer tokens by `show` for a definition.",
        "Answered in fewer tokens by `uses` for every use."
      )
    }

    test("source tools name a language, what reads and what searches it, and suffixes from `.`") {
      (
        SourceTools.of("Scala", Vector("scala"), "r", "s"),
        SourceTools.of("Scala", Vector.empty, "r", "s"),
        SourceTools.of(" ", Vector(".scala"), "r", "s")
      ) ==> (
        Left("a source's suffixes each start with `.`: scala"),
        Left("a source's suffixes each start with `.`: none"),
        Left("source tools name their language, and what reads and what searches it")
      )
    }
  }
}
