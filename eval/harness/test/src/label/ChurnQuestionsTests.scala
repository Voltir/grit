package grit.eval.harness.label

import grit.eval.harness.capture.CaseId

import utest.*

/** `questions.json` read: questions by id, each its gold and its asking in each round. */
object ChurnQuestionsTests extends TestSuite {

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)

  private val gold = """{"facts": [["x"]], "never": []}"""

  private def file(asked: (String, String)*): String =
    asked
      .map((q, rounds) => s""""$q": {"gold": $gold, "asked": {$rounds}}""")
      .mkString("{", ", ", "}")

  val tests = Tests {
    test("paired keeps the questions asked in both rounds, in file order, each its two messages") {
      ChurnQuestions
        .read(
          file(
            "q2" -> """"a": "CA/2.1", "b": "CB/2.1"""",
            "q3" -> """"a": "CA/3.1"""",
            "q1" -> """"b": "CB/1.1", "a": "CA/1.1", "c": "CC/1.1""""
          )
        )
        .flatMap(_.paired("a", "b")) ==> Right(
        Vector(
          "q2" -> ChurnPair(id("CA/2.1"), id("CB/2.1")),
          "q1" -> ChurnPair(id("CA/1.1"), id("CB/1.1"))
        )
      )
    }

    test("a round no question was asked in, or none asked in both, is refused by name") {
      val qs = ChurnQuestions
        .read(file("q1" -> """"a": "CA/1.1"""", "q2" -> """"b": "CB/2.1""""))
        .fold(sys.error, identity)
      Vector(qs.paired("a", "z"), qs.paired("a", "b")) ==> Vector(
        Left("churn questions: no question was asked in round z"),
        Left("churn questions: no question was asked in both rounds a and b")
      )
    }

    test("a message asking two questions, or one twice, is refused by its id") {
      Vector(
        file("q1" -> """"a": "CA/1.1"""", "q2" -> """"b": "CA/1.1""""),
        file("q1" -> """"a": "CA/1.1", "b": "CA/1.1"""")
      ).map(ChurnQuestions.read) ==> Vector.fill(2)(
        Left("churn questions: CA/1.1 asks more than once")
      )
    }

    test("a gold with no fact, or a fact with no string, is refused naming its question") {
      Vector(
        """{"q1": {"gold": {"facts": [], "never": []}, "asked": {}}}""",
        """{"q1": {"gold": {"facts": [["x"], []], "never": []}, "asked": {}}}"""
      ).map(ChurnQuestions.read) ==> Vector(
        Left("churn questions: q1: its gold holds no fact"),
        Left("churn questions: q1: a fact holds no string")
      )
    }

    test("an asking that is no message's id is refused naming its question and round") {
      ChurnQuestions.read(file("q1" -> """"a": "CA-1.1"""")) ==> Left(
        "churn questions: q1: asked: a: not a case id: CA-1.1"
      )
    }
  }
}
