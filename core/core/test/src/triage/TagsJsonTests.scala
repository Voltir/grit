package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.{Answer, AnswersJson}
import grit.core.id.QuestionName
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.store.PayloadJson

import utest.*

/** [[TagsJson]] and [[GateJson]]: recorded forms, read back by every later build. */
object TagsJsonTests extends TestSuite {

  private def p(x: Double) = Probability.clamped(x)
  private def name(text: String) = QuestionName.of(text).getOrElse(sys.error(text))

  private val Spent: Usage =
    Usage(Tokens(640), Tokens(0), Tokens.Zero, Some(BigDecimal("0.00002688")))

  private val decision: Tags.Weighed = Tags.Weighed(
    VectorMap(
      name("gap") -> Answer
        .choice(Vector(Answer.Weight("asks", 0.0), Answer.Weight("closes", 1.0)))
        .getOrElse(sys.error("a choice")),
      name("open") -> Answer.YesNo(0.125)
    ),
    "jev-1.13.0",
    Spent
  )

  val tests = Tests {
    test("tags are journaled by name, and an earlier build's four probabilities read as v1's") {
      // A triage in flight across the change reads back what the earlier build recorded.
      val earlier = ujson.read(
        """{"kind":"decision","kindP":0.75,"waiting":0.125,"durable":0.875,"helps":0.25,""" +
          """"model":"jev-1.13.0","usage":""" + PayloadJson.writeUsage(Spent).render() + "}"
      )
      TagsJson.read(earlier) ==> Right(
        Tags.Weighed(
          Tags.V1.answers(Kind.Decision, p(0.75), p(0.125), p(0.875), p(0.25)),
          "jev-1.13.0",
          Spent
        )
      )
      TagsJson.write(decision)("answers") ==> AnswersJson.writeNamed(decision.answers)
      TagsJson.read(TagsJson.write(decision)).map {
        case Tags.Weighed(answers, model, usage) => (answers.toVector, model, usage)
        case other => other
      } ==> Right((decision.answers.toVector, "jev-1.13.0", Spent))
      TagsJson.read(TagsJson.write(Tags.Unanswered("unavailable"))) ==>
        Right(Tags.Unanswered("unavailable"))
    }

    test("a gate's result is recorded in a fixed form, and read back as written") {
      // Pinned: a turn's recorded offer holds these, and every later build reads them.
      val open = Reading.Yes(name("open"))
      val asks = Reading.Key(name("gap"), "asks")
      val fails = Gate.Checked.Fails(
        Gate.Failed(Bound.AtLeast(asks, p(0.5)), p(0.0)),
        Vector(Gate.Failed(Bound.Below(open, p(0.5)), p(0.75)))
      )
      GateJson.writeChecked(Gate.Checked.Passes) ==> ujson.Str("passes")
      GateJson.writeChecked(fails).render() ==>
        """{"fails":[{"reading":{"reads":"key","name":"gap","key":"asks"},"bound":"at_least","p":0.5,"read":0},""" +
        """{"reading":{"reads":"yes","name":"open"},"bound":"below","p":0.5,"read":0.75}]}"""
      GateJson.writeChecked(Gate.Checked.Unread(open)).render() ==>
        """{"unread":{"reads":"yes","name":"open"}}"""
      Vector(Gate.Checked.Passes, fails, Gate.Checked.Unread(asks))
        .map(c => GateJson.readChecked(GateJson.writeChecked(c))) ==>
        Vector(Right(Gate.Checked.Passes), Right(fails), Right(Gate.Checked.Unread(asks)))
      GateJson.readChecked(ujson.Obj("fails" -> ujson.Arr())) ==>
        Left("checked: fails names no bound")
    }
  }
}
