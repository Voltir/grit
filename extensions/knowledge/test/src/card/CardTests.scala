package grit.knowledge.card

import java.time.Instant

import grit.core.document.{DocText, Pointer}
import grit.core.id.{CorpusName, DocKey}
import grit.core.tool.ToolName

import utest.*

object CardTests extends TestSuite {

  private def right[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private val corpus = right(CorpusName.of("repo"))
  private val version = right(UnitVersion.of("3068d045f21e723190d01fed918ed7e6651bf59a"))
  private val pointer =
    right(Pointer.of(ToolName("github_get_file_contents"), "path" -> ujson.Str("docs/testing.md")))

  private def card(terms: Vector[String]): Card =
    Card.file(
      corpus,
      "docs/testing.md",
      version,
      right(CardSummary.of("How the project is tested.", terms)),
      pointer
    )

  private val changed = Instant.parse("2026-10-14T09:30:00Z")

  private def drive(title: Option[String]): Card =
    Card.file(
      corpus,
      "docs/testing.md",
      UnitVersion.Changed(changed),
      right(CardSummary.of("How the project is tested.", Vector("unit tests"))),
      pointer,
      title
    )

  val tests = Tests {
    test("a card's key is the SHA-256 of its corpus, a newline and its path, in hex") {
      val key = card(Vector.empty).key
      assert(
        CardKey.value(key) == "2d08a29b84eb05c0538882c21a84fa54432a8aa10c0bde0effa4458528a4fabb"
      )
      assert(CardKey.parse(CardKey.value(key)) == Right(key))
      assert(DocKey.value(card(Vector.empty).docKey) == CardKey.value(key))
    }

    test("a path over 200 characters is hashed whole") {
      val long = "src/" + ("deep/" * 60) + "file.scala"
      assert(long.length > 200)
      assert(CardKey.of(corpus, long) != CardKey.of(corpus, long + "x"))
    }

    // The path leads so a reader of the window, or the relevance judge, knows which file a card
    // is of without reading its pointer.
    test("a card's text is its path, its summary, then a line of its terms") {
      val text = DocText.value(card(Vector("unit tests", "end-to-end")).text)
      assert(
        text == "docs/testing.md\n\nHow the project is tested.\n\nTerms: unit tests, end-to-end"
      )
    }

    test("a card with no terms is its path and its summary alone") {
      assert(
        DocText.value(card(Vector.empty).text) == "docs/testing.md\n\nHow the project is tested."
      )
    }

    test("a card's data holds its kind, path, terms and version") {
      val data = card(Vector("unit tests")).data
      assert(
        data == ujson.Obj(
          "kind" -> "file",
          "path" -> "docs/testing.md",
          "terms" -> ujson.Arr("unit tests"),
          "version" -> "3068d045f21e723190d01fed918ed7e6651bf59a"
        )
      )
    }

    test("a directory's card's data holds the scope it was made under too") {
      val scope = Scope.of(Set("include docs", "extension md"))
      val dir = Card.directory(
        corpus,
        "docs",
        version,
        scope,
        right(CardSummary.of("The directory docs.", Vector.empty)),
        pointer
      )
      assert(
        dir.data == ujson.Obj(
          "kind" -> "directory",
          "path" -> "docs",
          "terms" -> ujson.Arr(),
          "version" -> "3068d045f21e723190d01fed918ed7e6651bf59a",
          "scope" -> Scope.value(scope)
        )
      )
    }

    test("a scope is the SHA-256 of its terms sorted, one to a line, whatever their order") {
      // printf 'extension md\ninclude docs' | sha256sum
      val pinned = "367673b8c5a2e8a980174976b881e4fa49f6e5064f3775993db49cc7a5206e10"
      val scope = Scope.of(Set("include docs", "extension md"))
      assert(Scope.value(scope) == pinned, Scope.parse(pinned) == Right(scope))
    }

    test("a key read back must be 64 lowercase hex digits") {
      assert(CardKey.parse("2D08" + "a" * 60).isLeft)
      assert(CardKey.parse("a" * 63).isLeft)
    }

    test("a version is a git SHA of 40 or 64 lowercase hex digits") {
      assert(UnitVersion.of("a" * 40).isRight)
      assert(UnitVersion.of("a" * 64).isRight)
      assert(UnitVersion.of("a" * 41).isLeft)
      assert(UnitVersion.of("A" * 40).isLeft)
      assert(UnitVersion.sha("a" * 40) == UnitVersion.of("a" * 40))
      assert(UnitVersion.sha(UnitVersion.value(UnitVersion.Changed(changed))).isLeft)
    }
    test("a version is read back by its written form, a SHA or an instant") {
      val sha = UnitVersion.of("a" * 40)
      val at = UnitVersion.Changed(changed)
      assert(
        UnitVersion.value(at) == "2026-10-14T09:30:00Z",
        UnitVersion.of(UnitVersion.value(at)) == Right(at),
        sha.map(UnitVersion.value) == Right("a" * 40),
        UnitVersion.of("2026-10-14").isLeft
      )
    }
    // Its path follows its title: a Drive card's path is its file's id, which a title alone
    // would not name.
    test("a Changed card's text begins with its title, its path and the day it changed") {
      val text = DocText.value(drive(Some("Budget 2027")).text)
      assert(
        text == "Budget 2027 (docs/testing.md), last changed 2026-10-14\n\n" +
          "How the project is tested.\n\nTerms: unit tests"
      )
    }
    // Its path, not its kind, now that every card's first line names its path.
    test("a Changed card with no title begins with its path") {
      val text = DocText.value(drive(None).text)
      assert(
        text.startsWith("docs/testing.md, last changed 2026-10-14\n\nHow the project is tested.")
      )
    }
    test("a title is its first 80 characters, its line breaks made spaces") {
      val text = DocText.value(drive(Some("a\nb" + "c" * 100)).text)
      assert(text.startsWith("a b" + "c" * 77 + " (docs/testing.md), last changed 2026-10-14\n"))
    }
    test("a Changed card's data holds its title and the day it changed too") {
      assert(
        drive(Some("Budget 2027")).data == ujson.Obj(
          "kind" -> "file",
          "path" -> "docs/testing.md",
          "terms" -> ujson.Arr("unit tests"),
          "version" -> "2026-10-14T09:30:00Z",
          "changed" -> "2026-10-14",
          "title" -> "Budget 2027"
        )
      )
    }
    // A title now shows in every card's first line, not in a Changed card's alone.
    test("a Sha card's text begins with its title and path, with no day") {
      val titled = Card.file(
        corpus,
        "docs/testing.md",
        version,
        right(CardSummary.of("How the project is tested.", Vector.empty)),
        pointer,
        Some("Testing")
      )
      assert(
        DocText.value(titled.text) == "Testing (docs/testing.md)\n\nHow the project is tested.",
        titled.data == ujson.Obj(
          "kind" -> "file",
          "path" -> "docs/testing.md",
          "terms" -> ujson.Arr(),
          "version" -> "3068d045f21e723190d01fed918ed7e6651bf59a",
          "title" -> "Testing"
        )
      )
    }
  }
}
