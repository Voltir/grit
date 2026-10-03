package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.period.Probability

import utest.*

/** v2's draft gate, rebuilt from its named parts, against the bound list it replaced, checked
  * by the algorithm that list was checked by. A held message records the gate's result
  * (`Silence.Gated`, `Silence.Unasked`), so where both answer the same, the record is the
  * same bytes.
  */
object DraftGateIdentityTests extends TestSuite {

  private val half = Probability.clamped(0.5)

  /** v2's gate as a bound list, as triage drafted by it before the parts. */
  private val v2Bounds: Vector[Bound] = Vector(
    Bound.AtLeast(Reading.Key(Tags.V2.gap, "asks"), half),
    Bound.AtLeast(Reading.Yes(Tags.V2.open), half),
    Bound.Below(Reading.Yes(Tags.V2.to), half),
    Bound.Below(Reading.Yes(Tags.V2.anchor), half)
  )

  /** v1's gate as a bound list. */
  private val v1Bounds: Vector[Bound] = Vector(
    Bound.Below(Reading.Chosen(Tags.V1.kind, Kind.written(Kind.Chatter)), half),
    Bound.AtLeast(Reading.Yes(Tags.V1.helps), half)
  )

  /** The algorithm a bound list was checked by, frozen as the oracle: `Unread` at the first
    * bound unanswered in kind when no bound before it fails; otherwise `Fails` with the first
    * failing bound and every later one that fails, a later unread bound passed over; otherwise
    * `Passes`.
    */
  private def oracle(
      bounds: Vector[Bound],
      answers: VectorMap[QuestionName, Answer]
  ): Gate.Checked = {
    def holds(b: Bound, read: Probability) = b match {
      case Bound.AtLeast(_, p) => read >= p
      case Bound.Below(_, p) => !(read >= p)
    }
    val read = bounds.map(b => b -> b.reading.in(answers))
    def failed(from: Vector[(Bound, Option[Probability])]) =
      from.collect { case (b, Some(v)) if !holds(b, v) => Gate.Failed(b, v) }
    read.zipWithIndex
      .collectFirst {
        case ((b, None), _) => Gate.Checked.Unread(b.reading)
        case ((b, Some(v)), i) if !holds(b, v) =>
          Gate.Checked.Fails(Gate.Failed(b, v), failed(read.drop(i + 1)))
      }
      .getOrElse(Gate.Checked.Passes)
  }

  private def outcome(c: Gate.Checked): String = c match {
    case Gate.Checked.Passes => "passes"
    case Gate.Checked.Fails(_, _) => "fails"
    case Gate.Checked.Unread(_) => "unread"
  }

  /** Each of v2's gate readings unanswered, answered in the other kind, or answered at 0,
    * either side of one half, at one half, or at 1: every edge of every bound.
    */
  private val grid: Vector[VectorMap[QuestionName, Answer]] = {
    val values: Vector[Option[Option[Double]]] =
      None +: Some(None) +: Vector(0.0, 0.49, 0.5, 0.51, 1.0).map(v => Some(Some(v)))
    def choice(v: Double) =
      Answer.Choice("asks", Vector(Answer.Weight("asks", v), Answer.Weight("nothing", 1 - v)), 0.0)
    def gap(at: Option[Option[Double]]) =
      at.map(v => Tags.V2.gap -> v.fold(Answer.YesNo(0.9))(choice))
    def yes(name: QuestionName, at: Option[Option[Double]]) =
      at.map(v => name -> v.fold(choice(0.9))(Answer.YesNo(_)))
    for { g <- values; o <- values; t <- values; a <- values } yield VectorMap.from(
      gap(g).toVector ++ yes(Tags.V2.open, o) ++ yes(Tags.V2.to, t) ++ yes(Tags.V2.anchor, a)
    )
  }

  private def partial(answers: VectorMap[QuestionName, Answer]) =
    Tags.V2.drafts.reads.exists(_.in(answers).isEmpty)

  /** Whether `answers` answer some of `gate`'s readings and not others. */
  private def someNotAll(gate: Gate, answers: VectorMap[QuestionName, Answer]) = {
    val answered = gate.reads.count(_.in(answers).isDefined)
    answered > 0 && answered < gate.reads.size
  }

  val tests = Tests {
    test(
      "on every recorded answer set, v2's and v1's, v2's draft gate and v1's gate check exactly as the bound lists they replaced"
    ) {
      val recorded = RecordedAnswers.V2 ++ RecordedAnswers.V1
      recorded.filterNot(a =>
        Tags.V2.drafts.check(a) == oracle(v2Bounds, a) && Tags.V1.gate.check(a) == oracle(
          v1Bounds,
          a
        )
      ) ==> Vector.empty
      // The fixture reaches every outcome: v2's gate passes and fails on v2's answers and is
      // unread on v1's; v1's gate fails and passes on v1's. No recorded set answers some of
      // either gate's readings and not others, where the two rules may give different reasons.
      (
        RecordedAnswers.V2.groupMapReduce(a => outcome(Tags.V2.drafts.check(a)))(_ => 1)(_ + _),
        RecordedAnswers.V1.groupMapReduce(a => outcome(Tags.V2.drafts.check(a)))(_ => 1)(_ + _),
        RecordedAnswers.V1.groupMapReduce(a => outcome(Tags.V1.gate.check(a)))(_ => 1)(_ + _),
        recorded.count(a => someNotAll(Tags.V2.drafts, a) || someNotAll(Tags.V1.gate, a))
      ) ==> (
        Map("passes" -> 7, "fails" -> 72),
        Map("unread" -> 79),
        Map("passes" -> 45, "fails" -> 34),
        0
      )
    }

    test(
      "on every grid point answering each of v2's gate readings, the rebuilt gate checks exactly as the bound list"
    ) {
      val whole = grid.filterNot(partial)
      (whole.size, whole.filterNot(a => Tags.V2.drafts.check(a) == oracle(v2Bounds, a))) ==>
        (625, Vector.empty)
    }

    test(
      "on every grid point leaving one of v2's gate readings unread, both hold the message, and a reason differs only where a failing bound now decides what an earlier unread one left Unasked"
    ) {
      val partly = grid.filter(partial)
      val checked = partly.map(a => (Tags.V2.drafts.drafts(a), oracle(v2Bounds, a)))
      (
        partly.size,
        checked.count((now, _) => now.contains(true)),
        checked.count((_, was) => was == Gate.Checked.Passes),
        partly
          .map(a => (oracle(v2Bounds, a), Tags.V2.drafts.check(a)))
          .collect { case (was, now) if was != now => (outcome(was), outcome(now)) }
          .distinct
      ) ==> (1776, 0, 0, Vector(("unread", "fails")))
    }
  }
}
