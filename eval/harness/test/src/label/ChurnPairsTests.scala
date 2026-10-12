package grit.eval.harness.label

import grit.eval.harness.capture.CaseId

import utest.*

/** `churn-pairs.json` read: questions by id, each a before and an after message. */
object ChurnPairsTests extends TestSuite {

  private def id(s: String): CaseId = CaseId.read(s).fold(sys.error, identity)

  val tests = Tests {
    test("a file's questions are read in its order, each its before and after message") {
      ChurnPairs
        .read(
          """{"q2": {"before": "CB/2.1", "after": "CA/2.1"},
            | "q1": {"before": "CB/1.1", "after": "CA/1.1"}}""".stripMargin
        )
        .map(_.questions.toVector) ==> Right(
        Vector(
          "q2" -> ChurnPair(id("CB/2.1"), id("CA/2.1")),
          "q1" -> ChurnPair(id("CB/1.1"), id("CA/1.1"))
        )
      )
    }

    test("a message asking two questions, or one twice, is refused by its id") {
      Vector(
        """{"q1": {"before": "CB/1.1", "after": "CA/1.1"},
          | "q2": {"before": "CB/1.1", "after": "CA/2.1"}}""".stripMargin,
        """{"q1": {"before": "CB/1.1", "after": "CB/1.1"}}"""
      ).map(ChurnPairs.read) ==> Vector.fill(2)(Left("churn pairs: CB/1.1 asks more than once"))
    }

    test("a side that is no message's id, or missing, is refused naming its question") {
      Vector(
        """{"q1": {"before": "CB-1.1", "after": "CA/1.1"}}""",
        """{"q1": {"before": "CB/1.1"}}"""
      ).map(ChurnPairs.read(_).isLeft) ==> Vector(true, true)
      ChurnPairs
        .read("""{"q1": {"before": "CB-1.1", "after": "CA/1.1"}}""")
        .left
        .map(
          _.startsWith("churn pairs: q1: before: ")
        ) ==> Left(true)
    }
  }
}
