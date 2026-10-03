package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.period.Probability

import utest.*

object TagsTests extends TestSuite {

  private def p(x: Double) = Probability.clamped(x)
  private def name(text: String) = QuestionName.of(text).getOrElse(sys.error(text))

  val tests = Tests {
    test(
      "v1's tags as kept before they were named are its answers under its names, kind a choice weighing only its kind"
    ) {
      Tags.V1.answers(Kind.Decision, p(0.75), p(0.125), p(0.625), p(0.375)) ==> VectorMap(
        name("kind") -> Answer.Choice("decision", Vector(Answer.Weight("decision", 0.75)), 1.0),
        name("waiting") -> Answer.YesNo(0.125),
        name("durable") -> Answer.YesNo(0.625),
        name("helps") -> Answer.YesNo(0.375)
      )
    }
  }
}
