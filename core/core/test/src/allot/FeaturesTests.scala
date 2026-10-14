package grit.core.allot

import grit.core.classify.{Answer, Ask, Criterion}
import grit.core.period.Probability

import utest.*

object FeaturesTests extends TestSuite {

  private val kindAsk: Ask[Unit, grit.core.classify.Decision[TaskKind]] = {
    val all = TaskKind.values.toVector.map(k => Criterion(k, k.word, None))
    Ask
      .choice[Unit, TaskKind]("kind?", all(0), all(1), all.drop(2)*)
      .fold(d => throw new java.lang.AssertionError(d.toString), identity)
  }

  private def decision(weights: (TaskKind, Double)*) =
    kindAsk
      .read(
        Vector(
          Answer.Choice(
            weights.maxBy(_._2)._1.word,
            weights.toVector.map((k, p) => Answer.Weight(k.word, p)),
            0.5
          )
        )
      )
      .fold(e => throw new java.lang.AssertionError(e.toString), identity)

  private def p(d: Double) = Probability.clamped(d)

  val tests = Tests {
    test("from keeps each kind's weight as decided and clamps hard") {
      val f = Features.from(decision(TaskKind.Coding -> 0.75, TaskKind.Lookup -> 0.25), 1.2)
      f.kinds.toVector ==> TaskKind.values.toVector.map {
        case TaskKind.Coding => TaskKind.Coding -> p(0.75)
        case TaskKind.Lookup => TaskKind.Lookup -> p(0.25)
        case k => k -> Probability.Zero
      }
      f.hard ==> Probability.One
      f.kind ==> TaskKind.Coding
    }

    test("a root's kind is its most probable, the first in order on a tie") {
      Features
        .of(Map(TaskKind.Writing -> p(0.4), TaskKind.Lookup -> p(0.4)), p(0.1))
        .map(_.kind) ==>
        Some(TaskKind.Lookup)
      Features.of(Map(TaskKind.Decision -> p(0.6), TaskKind.Chat -> p(0.4)), p(0.1)).map(_.kind) ==>
        Some(TaskKind.Decision)
    }

    test("of with no kind weighing above 0 is none") {
      Features.of(Map(TaskKind.Writing -> Probability.Zero), p(0.5)) ==> None
      Features.of(Map.empty, p(0.5)) ==> None
    }
  }
}
