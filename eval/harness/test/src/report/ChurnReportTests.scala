package grit.eval.harness.report

import grit.core.id.WorkflowId
import grit.eval.harness.score.Churn
import grit.eval.harness.score.Fixtures.id
import grit.eval.harness.stats.{Estimate, Proportion}

import utest.*

/** A churn comparison as its report prints it: ids and numbers alone. */
object ChurnReportTests extends TestSuite {

  import Churn.Reads

  private def side(w: String, reads: Reads, confounded: Boolean = false) =
    Churn.Side(id(s"C1/$w"), WorkflowId(s"$w:0"), reads, confounded)

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
    Proportion.of(Vector("q1" -> true, "q2" -> false))
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
          "pairs: 2; confounded turns: 1 (a1:0)",
          "never read: before 1, after 0; never useful: before 1, after 1",
          "after-turns whose first read followed a card their window held: 0.500 (1/2 in 2 " +
            "questions) too few questions for an interval"
        )
      // t.975 and t.80 on 1 df: 12.706 and 1.376.
      lines.filter(_.startsWith("| indexed")) ==> Vector(
        "| indexed | 2 | 1.000 | [-11.706, 13.706] | 14.083 |",
        "| indexed, unconfounded | — | — | — | — |"
      )
      lines.dropWhile(_ != "## Pairs").drop(4) ==> Vector(
        "| q1 | b1:0 2, useful 3 | a1:0 0 followed, useful 0 (confounded) |",
        "| q2 | b2:0 unread | a2:0 1, useful never |"
      )
    }
  }
}
