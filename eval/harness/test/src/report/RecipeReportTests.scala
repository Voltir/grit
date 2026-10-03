package grit.eval.harness.report

import scala.collection.immutable.VectorMap

import grit.core.id.{ConversationId, WorkflowId}
import grit.eval.harness.reply.Judged
import grit.eval.harness.score.{Priced, Rate, Recipe, Shift}
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
    // A ninth of the input cached, its share at a tenth of the input rate: 10% off.
    Priced(8356, 2.25612e-3, 2.5068e-3, Some(2.5068e-4), Proportion(1000, 9000, 1, few), 2, 0),
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
        VectorMap.empty,
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
          VectorMap.empty,
          Varied("shipped", recipe(Proportion(0, 0, 0, few)), Vector.empty),
          Vector(Varied("offer-0.3", recipe(Proportion(0, 0, 0, few)), judged))
        )
        .linesIterator
        .find(_.startsWith("| offer-0.3 | 1 |")) ==> Some(
        "| offer-0.3 | 1 | 12 → 2 | 4478 → 300 | 4178 | 0.500 (1/2 in 1 thread) too few threads for an interval |"
      )
    }

    test("the tools withheld are priced in mills, with their range, hit rate and the cache's cut") {
      val report = Report
        .recipes(
          "20261005",
          Vector.empty,
          VectorMap(
            "m/a" -> Right(Rate(3e-7, Some(3e-8), 2.5e-6, 52, 1, 0.0004)),
            "jev" -> Left("1 priced row, fewer than the 2 prices read from them")
          ),
          Varied("shipped", recipe(Proportion(0, 0, 0, few)), Vector.empty),
          Vector(Varied("offer-0.3", recipe(Proportion(0, 0, 0, few)), judged))
        )
        .linesIterator
        .toVector
      val price = report.dropWhile(_ != "## Price of the tools withheld")
      price.find(_.startsWith("| offer-0.3 |")) ==> Some(
        "| offer-0.3 | 2 | 0.111 (1000/9000 in 1 thread) too few threads for an interval | 8356 | " +
          "2.3 mills | 0.25 mills to 2.5 mills | 10% | 90% | 0 |"
      )
      price.filter(l => l.startsWith("| m/a ") || l.startsWith("| jev ")) ==> Vector(
        "| m/a | 0.3000 | 0.03000 | 2.500 | 52 | 1 | 0.0% |",
        "| jev | none: 1 priced row, fewer than the 2 prices read from them | — | — | — | — | — |"
      )
    }

    test(
      "the synthetic reference: each variant's pass rate by case, and its failing cases by name"
    ) {
      def turns(buried: Judged) = Vector(
        CaseJudged("a/plain", "a", Judged.Pass),
        CaseJudged("a/buried", "a", buried),
        CaseJudged("b/plain", "b", Judged.Pass)
      )
      Report
        .synthetic(
          Vector("cases: 2 in 2 variants"),
          Vector("shipped" -> turns(Judged.Fail), "wide-addressed" -> turns(Judged.Pass))
        )
        .linesIterator
        .filter(_.startsWith("| "))
        .toVector ==> Vector(
        "| variant | rate | unjudged | failed |",
        "| shipped | 0.667 (2/3 in 2 cases) too few cases for an interval | 0 | a/buried |",
        "| wide-addressed | 1.000 (3/3 in 2 cases) too few cases for an interval | 0 | none |"
      )
    }
  }
}
