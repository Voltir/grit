package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName

import utest.*

/** v3's draft gate and its directed branch, over answers each named case sets apart: one
  * reading moved from the case that drafts by each branch.
  */
object V3GateTests extends TestSuite {

  private def answers(
      asks: Double,
      open: Double,
      to: Double,
      toGrit: Double,
      anchor: Double
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
      Tags.V2.anchor -> Answer.YesNo(anchor)
    )

  /** (drafts, directed) for `a`. */
  private def read(a: VectorMap[QuestionName, Answer]) =
    (Tags.V3.drafts.drafts(a), Tags.V3.directed.drafts(a))

  val tests = Tests {
    test("a question to grit by name drafts as directed, whoever else it reads as to") {
      // Nick's message as triage v2 read it, with to-grit as it would read: to 0.88 held it.
      read(answers(asks = 1.0, open = 0.93, to = 0.88, toGrit = 0.9, anchor = 0.13)) ==>
        (Some(true), Some(true))
    }

    test("a question to someone else is held, and is not directed") {
      read(answers(asks = 1.0, open = 0.93, to = 0.88, toGrit = 0.1, anchor = 0.13)) ==>
        (Some(false), Some(false))
    }

    test("a question to the room drafts, but not as directed at grit") {
      read(answers(asks = 1.0, open = 0.93, to = 0.1, toGrit = 0.1, anchor = 0.13)) ==>
        (Some(true), Some(false))
    }

    test("a question to grit that no record could answer drafts; one to the room is held") {
      read(answers(asks = 1.0, open = 0.93, to = 0.88, toGrit = 0.9, anchor = 0.9)) ==>
        (Some(true), Some(true))
      read(answers(asks = 1.0, open = 0.93, to = 0.1, toGrit = 0.1, anchor = 0.9)) ==>
        (Some(false), Some(false))
    }

    test("grit named in a message asking nothing, or asking what is answered, is held") {
      read(answers(asks = 0.1, open = 0.93, to = 0.88, toGrit = 0.9, anchor = 0.13)) ==>
        (Some(false), Some(false))
      read(answers(asks = 1.0, open = 0.1, to = 0.88, toGrit = 0.9, anchor = 0.13)) ==>
        (Some(false), Some(false))
    }

    test("answers of v2, without to-grit, are decided by the room's branch or unread") {
      val v2 =
        answers(asks = 1.0, open = 0.93, to = 0.1, toGrit = 0, anchor = 0.13) - Tags.V3.toGrit
      val toSomeone = v2.updated(Tags.V2.to, Answer.YesNo(0.88))
      (Tags.V3.drafts.drafts(v2), Tags.V3.drafts.drafts(toSomeone)) ==> (Some(true), None)
    }

    test(
      "tags are directed at grit when v3's directed branch passes on their answers, and only then"
    ) {
      def weighed(a: VectorMap[QuestionName, Answer]) =
        Tags.Weighed(a, "jev", grit.core.message.Usage.Zero)
      val toGrit = answers(asks = 1.0, open = 0.93, to = 0.88, toGrit = 0.9, anchor = 0.13)
      val toRoom = answers(asks = 1.0, open = 0.93, to = 0.1, toGrit = 0.1, anchor = 0.13)
      Vector(
        weighed(toGrit),
        weighed(toRoom),
        weighed(toGrit - Tags.V3.toGrit),
        Tags.Unanswered("down")
      ).map(Tags.directed) ==> Vector(true, false, false, false)
    }

    test("a message held by the gate is held on each bound it failed, once") {
      val half = grit.core.period.Probability.clamped(0.5)
      def failed(b: Bound, read: Double) =
        Gate.Failed(b, grit.core.period.Probability.clamped(read))
      Tags.V3.drafts.check(
        answers(asks = 0.0, open = 0.25, to = 0.9, toGrit = 0.1, anchor = 0.1)
      ) ==>
        Gate.Checked.Fails(
          failed(Bound.AtLeast(Reading.Key(Tags.V2.gap, "asks"), half), 0.0),
          Vector(
            failed(Bound.AtLeast(Reading.Yes(Tags.V2.open), half), 0.25),
            failed(Bound.AtLeast(Reading.Yes(Tags.V3.toGrit), half), 0.1),
            failed(Bound.Below(Reading.Yes(Tags.V2.to), half), 0.9)
          )
        )
    }
  }
}
