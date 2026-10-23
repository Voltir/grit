package grit.knowledge.card

import utest.*

object CardSummaryTests extends TestSuite {

  private def reply(json: ujson.Value): Either[String, CardSummary] =
    CardSummary.Reply.schema
      .check(json, Set.empty)
      .left
      .map(_.toString)
      .flatMap(CardSummary.Reply.read)

  private def terms(n: Int): ujson.Arr = ujson.Arr.from((1 to n).map(i => ujson.Str(s"term $i")))

  val tests = Tests {
    test("a reply's summary over 60 words is cut after its 60th") {
      val words = (1 to 75).map(i => s"w$i")
      val read = reply(ujson.Obj("summary" -> words.mkString(" "), "terms" -> terms(2)))
      assert(read.map(_.summary) == Right(words.take(60).mkString(" ")))
      assert(read.map(_.terms) == Right(Vector("term 1", "term 2")))
    }

    test("a reply of 60 words keeps them all") {
      val words = (1 to 60).map(i => s"w$i").mkString(" ")
      assert(
        reply(ujson.Obj("summary" -> words, "terms" -> terms(0))).map(_.summary) == Right(words)
      )
    }

    test("a reply with a blank summary is refused") {
      val read = reply(ujson.Obj("summary" -> "  \n ", "terms" -> terms(1)))
      assert(read == Left("a summary is not blank"))
    }

    test("a reply of more than 8 terms does not conform, and a summary of 9 is refused") {
      def conforms(n: Int) = CardSummary.Reply.schema
        .check(ujson.Obj("summary" -> "A file.", "terms" -> terms(n)), Set.empty)
        .map(_ => ())
        .left
        .map(_.toString)
      (conforms(8), conforms(9)) ==> (
        Right(()),
        Left("Mismatch(terms,expected at most 8 items, got 9)")
      )
      assert(reply(ujson.Obj("summary" -> "A file.", "terms" -> terms(8))).isRight)
      assert(
        CardSummary.of("A file.", terms(9).value.toVector.flatMap(_.strOpt)) ==
          Left("a summary has at most 8 terms, not 9")
      )
    }

    test("a term over three words is cut to three; a blank one is left out") {
      val read = CardSummary.of("A file.", Vector(" one two three four ", "  ", "five"))
      assert(read.map(_.terms) == Right(Vector("one two three", "five")))
    }

    test("a reply without terms is read as no terms, its summary kept") {
      val read = reply(ujson.Obj("summary" -> "A file."))
      assert(read.map(_.summary) == Right("A file."))
      assert(read.map(_.terms) == Right(Vector.empty))
    }
  }
}
