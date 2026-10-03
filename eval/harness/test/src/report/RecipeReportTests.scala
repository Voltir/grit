package grit.eval.harness.report

import scala.collection.immutable.VectorMap

import grit.core.id.{ConversationId, WorkflowId}
import grit.eval.harness.reply.Judged
import grit.eval.harness.score.{Recipe, Shift}
import grit.eval.harness.stats.Proportion

import utest.*

/** The recipes report over one made-up variant. */
object RecipeReportTests extends TestSuite {

  private val few = Proportion.Interval.TooFewClusters
  private def recipe(used: Proportion) = Recipe(
    1,
    Some(Shift(12, 2)),
    Some(Shift(4478, 300)),
    4178,
    Proportion(1, 2, 1, few),
    1,
    VectorMap.empty,
    Some(Shift(600, 600)),
    used,
    used,
    Vector(WorkflowId("w1"))
  )
  private val judged: Vector[(WorkflowId, ConversationId, Judged)] =
    Vector((WorkflowId("w1"), ConversationId("c"), Judged.Fail))

  val tests = Tests {
    test("used-section recall is stated undefined when no reply used a part, else its rate") {
      def report(used: Proportion) = Report.recipes(
        "20261005",
        Vector.empty,
        Varied("shipped", recipe(used), Vector.empty),
        Vector(Varied("offer-0.3", recipe(used), judged))
      )
      val undefined = "Undefined: no reply used a part of its window, so no variant can lose one."
      report(Proportion(0, 0, 0, few)).linesIterator.contains(undefined) ==> true
      report(Proportion(1, 1, 1, few)).linesIterator.contains(undefined) ==> false
    }

    test("called-tool recall and tokens saved are reported by variant") {
      Report
        .recipes(
          "20261005",
          Vector.empty,
          Varied("shipped", recipe(Proportion(0, 0, 0, few)), Vector.empty),
          Vector(Varied("offer-0.3", recipe(Proportion(0, 0, 0, few)), judged))
        )
        .linesIterator
        .find(_.startsWith("| offer-0.3 | 1 |")) ==> Some(
        "| offer-0.3 | 1 | 12 → 2 | 4478 → 300 | 4178 | 0.500 (1/2 in 1 thread) too few threads for an interval |"
      )
    }
  }
}
