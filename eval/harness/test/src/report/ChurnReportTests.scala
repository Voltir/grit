package grit.eval.harness.report

import grit.core.id.WorkflowId
import grit.eval.harness.label.Gold
import grit.eval.harness.score.Churn
import grit.eval.harness.score.Fixtures.id
import grit.eval.harness.stats.{Estimate, Proportion}

import utest.*

/** A churn comparison as its report prints it: ids and numbers alone. */
object ChurnReportTests extends TestSuite {

  import Churn.Reads

  private def side(
      w: String,
      reads: Reads,
      confounded: Boolean = false,
      calls: Int = 0,
      echoed: Boolean = false,
      grade: Churn.Grade = Churn.Grade.Ungraded
  ) =
    Churn.Side(id(s"C1/$w"), WorkflowId(s"$w:0"), reads, calls, confounded, echoed, grade)

  // q1: before read after 2 calls, usefully after 3; after read at once, following a card, its
  // window holding another pair's thread. q2: before never read; after read after 1, never
  // usefully. Indexed over q1 (2) and a made-up second question (0): mean 1, se 1 on 1 df.
  private val churn = Churn(
    Vector(
      Churn.Pair(
        "q1",
        side("b1", Reads.Read(2, followed = false, Some(3))),
        side("a1", Reads.Read(0, followed = true, Some(0)), confounded = true)
      ),
      Churn.Pair("q2", side("b2", Reads.Unread), side("a2", Reads.Read(1, followed = false, None)))
    ),
    Estimate.clustered(Vector("q1" -> 2.0, "q9" -> 0.0)),
    None,
    None,
    None,
    Proportion.of(Vector("q1" -> true, "q2" -> false)),
    None
  )

  private val gold = Gold(
    ::(Gold.Fact(::("24 hours", Nil)), List(Gold.Fact(::("daily", Nil)))),
    Vector("$")
  )

  // q1: before right after 3 calls, after right after 1, echoed; q2: before no reply, after
  // right having read. Better, clean over q2 (1) and a made-up second question (-1): mean 0,
  // se 1 on 1 df.
  private val graded = churn.copy(
    pairs = Vector(
      Churn.Pair(
        "q1",
        side(
          "b1",
          Reads.Read(2, followed = false, Some(3)),
          calls = 3,
          grade = Churn.Grade.Graded(gold.matched("24 hours, daily"))
        ),
        side(
          "a1",
          Reads.Read(0, followed = true, Some(0)),
          calls = 1,
          echoed = true,
          grade = Churn.Grade.Graded(gold.matched("24 hours, daily"))
        )
      ),
      Churn.Pair(
        "q2",
        side("b2", Reads.Unread, grade = Churn.Grade.Unreplied),
        side(
          "a2",
          Reads.Read(1, followed = false, None),
          calls = 2,
          grade = Churn.Grade.Graded(gold.matched("24 hours, daily"))
        )
      )
    ),
    correctness = Some(
      Churn.Correctness(
        Proportion.of(Vector("q1" -> true, "q2" -> false)),
        Proportion.of(Vector("q1" -> true, "q2" -> true)),
        None,
        Estimate.clustered(Vector("q1" -> 0.0, "q2" -> 1.0)),
        None,
        Estimate.clustered(Vector("q2" -> 1.0, "q8" -> -1.0))
      )
    )
  )

  val tests = Tests {
    test(
      "a churn report gives each difference with its interval and MDE, the turns never reading by side, the rate following a card, and a line a question"
    ) {
      val lines = Report.churn("20261012", churn).linesIterator.toVector
      lines.filter(l =>
        l.startsWith("pairs: ") || l.startsWith("never ") || l.startsWith("after-")
      ) ==>
        Vector(
          "pairs: 2; confounded turns: 1 (a1:0); echoed turns: 0",
          "never read: before 1, after 0; never useful: before 1, after 1",
          "after-turns whose first read followed a card their window held: 0.500 (1/2 in 2 " +
            "questions) too few questions for an interval"
        )
      // t.975 and t.80 on 1 df: 12.706 and 1.376.
      lines.filter(_.startsWith("| indexed")) ==> Vector(
        "| indexed | 2 | 1.000 | [-11.706, 13.706] | 14.083 |",
        "| indexed, clean | — | — | — | — |"
      )
      lines.dropWhile(_ != "## Pairs").drop(4) ==> Vector(
        "| q1 | b1:0 2, useful 3 | a1:0 0 followed, useful 0 (confounded) |",
        "| q2 | b2:0 unread | a2:0 1, useful never |"
      )
    }

    test(
      "a graded report gives the share correct by side, calls to correct and better, the turns echoed, and each turn's counts"
    ) {
      val lines = Report.churn("20261012", graded).linesIterator.toVector
      lines.filter(l => l.startsWith("pairs: ") || l.startsWith("correct: ")) ==> Vector(
        "pairs: 2; confounded turns: 0; echoed turns: 1 (a1:0)",
        "correct: before 0.500 (1/2 in 2 questions) too few questions for an interval; " +
          "after 1.000 (2/2 in 2 questions) too few questions for an interval"
      )
      // Better over q1 (0) and q2 (1): mean 0.5, se 0.5 on 1 df.
      lines.filter(l => l.startsWith("| to correct") || l.startsWith("| better")) ==> Vector(
        "| to correct | — | — | — | — |",
        "| better | 2 | 0.500 | [-5.853, 6.853] | 7.041 |",
        "| to correct, clean | — | — | — | — |",
        "| better, clean | 2 | 0.000 | [-12.706, 12.706] | 14.083 |"
      )
      lines.dropWhile(_ != "## Pairs").drop(4) ==> Vector(
        "| q1 | b1:0 2, useful 3; 3 calls, right (facts 2/2, never 0) | " +
          "a1:0 0 followed, useful 0 (echoed); 1 calls, right (facts 2/2, never 0) |",
        "| q2 | b2:0 unread; 0 calls, no reply | a2:0 1, useful never; 2 calls, right (facts 2/2, never 0) |"
      )
    }

    test("a rate over ten questions gives Wilson's interval to three places") {
      // Each question's residual ±0.5: CR1 design effect 10/9, so Wilson's interval on 9 items.
      val followed = Proportion.of((1 to 10).map(q => s"q$q" -> (q <= 5)).toVector)
      Report
        .churn("20261012", churn.copy(followed = followed))
        .linesIterator
        .filter(_.startsWith("after-"))
        .toVector ==> Vector(
        "after-turns whose first read followed a card their window held: 0.500 (5/10 in 10 " +
          "questions) [0.227, 0.773]"
      )
    }

    test("a rate over one question counts it as 1 question") {
      Report
        .churn("20261012", churn.copy(followed = Proportion.of(Vector("q1" -> true))))
        .linesIterator
        .filter(_.startsWith("after-"))
        .toVector ==> Vector(
        "after-turns whose first read followed a card their window held: 1.000 (1/1 in 1 " +
          "question) too few questions for an interval"
      )
    }
  }
}
