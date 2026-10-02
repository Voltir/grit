package grit.eval.harness.score

import grit.eval.harness.label.{Labelled, Labels}

import utest.*
import Fixtures.*

/** The labelling order. Every id here is synthetic. */
object OrderTests extends TestSuite {

  /** Case n's durable at `d`, its spread on it `s`, over `repeats` repeats. */
  private def answers(each: (Int, Double, Double, Int)*): Answers =
    Answers(
      each.map { (n, d, s, repeats) =>
        id(s"C1/$n") -> Averaged(Triage(Map.empty, 0, d, 0), Triage(Map.empty, 0, s, 0), repeats)
      }.toMap,
      Map.empty
    )

  // Case 1 is labelled durable; the rest are not.
  private val labels =
    Labels(Map(id("C1/1") -> Labelled.Blank.copy(durable = Some(true))), Set("v1"))

  private val durable = Target.Tagged(Tag.Durable)

  val tests = Tests {
    test("by difference: the unlabelled first, each by |B − A| largest first, ties by id") {
      // |Δ|: 1 .4 (labelled), 2 .1, 3 .4, 4 .05; 5 is in A alone.
      val a =
        answers((1, 0.1, 0, 1), (2, 0.2, 0, 1), (3, 0.5, 0, 1), (4, 0.5, 0, 1), (5, 0.5, 0, 1))
      val b = answers((1, 0.5, 0, 1), (2, 0.3, 0, 1), (3, 0.9, 0, 1), (4, 0.45, 0, 1))
      Order.difference(durable, a, b, labels).map(_.written) ==>
        Vector("C1/3", "C1/2", "C1/4", "C1/1")
    }

    test("by spread: cases repeated at least twice, the unlabelled first, widest first") {
      // Spread: 1 .3 (labelled), 2 .1, 3 .2; 4 was answered once.
      val a = answers((1, 0, 0.3, 3), (2, 0, 0.1, 3), (3, 0, 0.2, 3), (4, 0, 0.9, 1))
      val cases = Vector(1, 2, 3, 4).map(n => caseOf(n, n))
      Order.spread(durable, cases, a, labels).map(_.written) ==> Vector("C1/3", "C1/2", "C1/1")
    }
  }
}
