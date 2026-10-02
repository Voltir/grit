package grit.eval.harness.score

import grit.core.triage.Kind
import grit.eval.harness.label.{Labelled, Labels}

import utest.*
import Fixtures.*

/** Two runs compared case by case. Every id here is synthetic. */
object ComparisonTests extends TestSuite {

  /** Answers giving each case `durable` and the kind weighed `kinds`. */
  private def answers(each: (Int, Double, Map[Kind, Double])*): Answers =
    Answers(
      each.map { (n, d, kinds) =>
        val t = Triage(kinds, 0, d, 0)
        id(s"C1/$n") -> Averaged(t, t, 1)
      }.toMap,
      Map.empty
    )

  private def labels(each: (Int, Labelled)*): Labels =
    Labels(each.map((n, l) => id(s"C1/$n") -> l).toMap, Set("v1"))

  private val q = Map(Kind.Question -> 1.0)
  private val d = Map(Kind.Decision -> 1.0)
  private val an = Map(Kind.Answer -> 1.0)

  val tests = Tests {
    test("a tag: fixed, broken and moved across the threshold, decided yes at it") {
      val a = answers((1, 0.3, q), (2, 0.7, q), (3, 0.4, q), (4, 0.2, q), (5, 0.9, q), (6, 0.5, q))
      val b = answers((1, 0.7, q), (2, 0.3, q), (3, 0.6, q), (4, 0.4, q), (6, 0.49, q))
      val l = labels(
        1 -> Labelled.Blank.copy(durable = Some(true)),
        2 -> Labelled.Blank.copy(durable = Some(true)),
        4 -> Labelled.Blank.copy(durable = Some(false)),
        // At .5, A decided yes; at .49, B no.
        6 -> Labelled.Blank.copy(durable = Some(false))
      )
      // 5 is in A alone: not compared.
      Comparison.tag(Tag.Durable, 0.5, a, b, l) ==>
        Changes(Vector(id("C1/1"), id("C1/6")), Vector(id("C1/2")), Vector(id("C1/3")), 1)
    }

    test("a kind changed but wrong both times is moved, not broken") {
      val a = answers((1, 0, d), (2, 0, d), (3, 0, q), (4, 0, q))
      val b = answers((1, 0, q), (2, 0, an), (3, 0, an), (4, 0, q))
      val l = labels(
        1 -> Labelled.Blank.copy(kind = Some(Kind.Question)),
        2 -> Labelled.Blank.copy(kind = Some(Kind.Question)),
        3 -> Labelled.Blank.copy(kind = Some(Kind.Question))
      )
      Comparison.kind(a, b, l) ==>
        Changes(Vector(id("C1/1")), Vector(id("C1/3")), Vector(id("C1/2")), 1)
    }
  }
}
