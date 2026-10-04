package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.period.Probability

import utest.*

/** v4's draft gate beside v3's directed branch, over answers as Jev read the synthetic
  * named-person cases each test names: whom a question is put to no longer holds it; what
  * no record could supply still does, unless it was put to grit. Each answer holds v3's
  * `anchor` at 0.9, so a gate that read it in place of `anchor-record` would hold them all.
  */
object V4GateTests extends TestSuite {

  private def answers(
      to: Double,
      toGrit: Double,
      anchorRecord: Double,
      asks: Double = 1.0,
      open: Double = 0.93,
      anchor: Double = 0.9
  ): VectorMap[QuestionName, Answer] =
    VectorMap(
      Tags.V2.gap -> Answer.Choice(
        "asks",
        Vector(Answer.Weight("asks", asks), Answer.Weight("nothing", 1 - asks)),
        0.0
      ),
      Tags.V2.open -> Answer.YesNo(open),
      Tags.V2.to -> Answer.YesNo(to),
      Tags.V3.toGrit -> Answer.YesNo(toGrit),
      Tags.V2.anchor -> Answer.YesNo(anchor),
      Tags.V4.anchorRecord -> Answer.YesNo(anchorRecord)
    )

  /** (drafts, directed) for `a`. */
  private def read(a: VectorMap[QuestionName, Answer]) =
    (Tags.V4.drafts.drafts(a), Tags.V3.directed.drafts(a))

  val tests = Tests {
    test("a fact asked of a named person drafts, not as directed at grit") {
      // "Hey Bob, do you know when the release freeze starts?": to 0.87 held it under v3.
      read(answers(to = 0.87, toGrit = 0.04, anchorRecord = 0.16)) ==> (Some(true), Some(false))
    }

    test("an opinion asked of a named person is held") {
      // "Bob, what do you think of the new logo?"
      read(answers(to = 0.86, toGrit = 0.08, anchorRecord = 0.95)) ==> (Some(false), Some(false))
    }

    test("a fact asked of the room drafts, not as directed at grit") {
      // "Does anyone know when the release freeze starts?"
      read(answers(to = 0.05, toGrit = 0.06, anchorRecord = 0.17)) ==> (Some(true), Some(false))
    }

    test("a question put to grit drafts as directed, even one no record could supply") {
      read(answers(to = 0.88, toGrit = 0.9, anchorRecord = 0.9)) ==> (Some(true), Some(true))
    }

    test("v3's answers, without anchor-record, are decided by to-grit or unread") {
      val toGrit = answers(to = 0.88, toGrit = 0.9, anchorRecord = 0) - Tags.V4.anchorRecord
      val toBob = answers(to = 0.87, toGrit = 0.04, anchorRecord = 0) - Tags.V4.anchorRecord
      (Tags.V4.drafts.drafts(toGrit), Tags.V4.drafts.drafts(toBob)) ==> (Some(true), None)
    }

    test("a message held by the gate is held on each bound it failed, once") {
      val half = Probability.clamped(0.5)
      def failed(b: Bound, read: Double) = Gate.Failed(b, Probability.clamped(read))
      Tags.V4.drafts.check(
        answers(asks = 0.0, open = 0.25, to = 0.1, toGrit = 0.1, anchorRecord = 0.9)
      ) ==>
        Gate.Checked.Fails(
          failed(Bound.AtLeast(Reading.Key(Tags.V2.gap, "asks"), half), 0.0),
          Vector(
            failed(Bound.AtLeast(Reading.Yes(Tags.V2.open), half), 0.25),
            failed(Bound.AtLeast(Reading.Yes(Tags.V3.toGrit), half), 0.1),
            failed(Bound.Below(Reading.Yes(Tags.V4.anchorRecord), half), 0.9)
          )
        )
    }
  }
}
