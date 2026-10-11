package grit.knowledge.card

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
    Card.of(
      corpus,
      "docs/testing.md",
      UnitKind.File,
      version,
      right(CardSummary.of("How the project is tested.", terms)),
      pointer
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

    test("a path over 200 characters has a stable key of 64 hex digits") {
      val long = "src/" + ("deep/" * 60) + "file.scala"
      assert(long.length > 200)
      val key = CardKey.of(corpus, long)
      assert(key == CardKey.of(corpus, long))
      assert(CardKey.value(key).length == 64)
      assert(key != CardKey.of(corpus, long + "x"))
    }

    test("a card's text is its summary, then a line of its terms") {
      val text = DocText.value(card(Vector("unit tests", "end-to-end")).text)
      assert(text == "How the project is tested.\n\nTerms: unit tests, end-to-end")
    }

    test("a card with no terms is its summary alone") {
      assert(DocText.value(card(Vector.empty).text) == "How the project is tested.")
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

    test("a key read back must be 64 lowercase hex digits") {
      assert(CardKey.parse("2D08" + "a" * 60).isLeft)
      assert(CardKey.parse("a" * 63).isLeft)
    }

    test("a version is a git SHA of 40 or 64 lowercase hex digits") {
      assert(UnitVersion.of("a" * 40).isRight)
      assert(UnitVersion.of("a" * 64).isRight)
      assert(UnitVersion.of("a" * 41).isLeft)
      assert(UnitVersion.of("A" * 40).isLeft)
    }
  }
}
