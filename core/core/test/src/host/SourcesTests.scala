package grit.core.host

import grit.core.host.Sources.Read

import utest.*

/** [[Sources.Scala]]: which shell reads read Scala's sources, and what the rule points at. */
object SourcesTests extends TestSuite {

  private def argv(words: String*): Argv =
    Argv.of(words.head, words.tail*).fold(e => throw new java.lang.AssertionError(e), identity)

  private val scala = Sources.Scala

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

    test("sources name their language and suffixes that each start with `.`") {
      (
        Sources.of("Scala", Vector("scala")),
        Sources.of("Scala", Vector.empty),
        Sources.of(" ", Vector(".scala")),
        Sources.of("Scala", Vector(".scala"))
      ) ==> (
        Left("a source's suffixes each start with `.`: scala"),
        Left("a source's suffixes each start with `.`: none"),
        Left("sources name their language"),
        Right(Sources.Scala)
      )
    }
  }
}
