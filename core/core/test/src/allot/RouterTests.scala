package grit.core.allot

import grit.core.message.Cost
import grit.core.model.{Effort, Grade}
import grit.core.period.Probability
import grit.core.spend.{DailyCap, Spend}

import utest.*

object RouterTests extends TestSuite {

  import Allotment.{Calls, Reasoning}
  import TaskKind.*

  private def features(kind: TaskKind, hard: Double): Features =
    Features
      .of(Map(kind -> Probability.clamped(0.9)), Probability.clamped(hard))
      .getOrElse(throw new java.lang.AssertionError(kind.toString))

  private val Doubled: Calls = Calls.Scaled.of(2).getOrElse(throw new java.lang.AssertionError())

  // Every input: each kind at p(hard) 0.59 and 0.6, at each headroom, then not answered.
  private val table: Vector[(Option[(TaskKind, Double)], Headroom, Allotment)] = Vector(
    (
      Some(Chat -> 0.59),
      Headroom.Ample,
      Allotment(Some(Grade.Standard), Reasoning.At(Effort.Low), Calls.Fewest)
    ),
    (
      Some(Chat -> 0.6),
      Headroom.Ample,
      Allotment(Some(Grade.Standard), Reasoning.At(Effort.Low), Calls.Fewest)
    ),
    (
      Some(Chat -> 0.59),
      Headroom.Low,
      Allotment(Some(Grade.Standard), Reasoning.At(Effort.Low), Calls.Fewest)
    ),
    (
      Some(Chat -> 0.6),
      Headroom.Low,
      Allotment(Some(Grade.Standard), Reasoning.At(Effort.Low), Calls.Fewest)
    ),
    (
      Some(Lookup -> 0.59),
      Headroom.Ample,
      Allotment(Some(Grade.Standard), Reasoning.Own, Calls.Default)
    ),
    (
      Some(Lookup -> 0.6),
      Headroom.Ample,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.High), Doubled)
    ),
    (
      Some(Lookup -> 0.59),
      Headroom.Low,
      Allotment(Some(Grade.Standard), Reasoning.AtMost(Effort.Medium), Calls.Default)
    ),
    (
      Some(Lookup -> 0.6),
      Headroom.Low,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.Medium), Calls.Default)
    ),
    (
      Some(Investigation -> 0.59),
      Headroom.Ample,
      Allotment(Some(Grade.Standard), Reasoning.Own, Calls.Default)
    ),
    (
      Some(Investigation -> 0.6),
      Headroom.Ample,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.High), Doubled)
    ),
    (
      Some(Investigation -> 0.59),
      Headroom.Low,
      Allotment(Some(Grade.Standard), Reasoning.AtMost(Effort.Medium), Calls.Default)
    ),
    (
      Some(Investigation -> 0.6),
      Headroom.Low,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.Medium), Calls.Default)
    ),
    (
      Some(Analysis -> 0.59),
      Headroom.Ample,
      Allotment(Some(Grade.Standard), Reasoning.Own, Calls.Default)
    ),
    (
      Some(Analysis -> 0.6),
      Headroom.Ample,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.High), Doubled)
    ),
    (
      Some(Analysis -> 0.59),
      Headroom.Low,
      Allotment(Some(Grade.Standard), Reasoning.AtMost(Effort.Medium), Calls.Default)
    ),
    (
      Some(Analysis -> 0.6),
      Headroom.Low,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.Medium), Calls.Default)
    ),
    (
      Some(Writing -> 0.59),
      Headroom.Ample,
      Allotment(Some(Grade.Standard), Reasoning.Own, Calls.Default)
    ),
    (
      Some(Writing -> 0.6),
      Headroom.Ample,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.High), Doubled)
    ),
    (
      Some(Writing -> 0.59),
      Headroom.Low,
      Allotment(Some(Grade.Standard), Reasoning.AtMost(Effort.Medium), Calls.Default)
    ),
    (
      Some(Writing -> 0.6),
      Headroom.Low,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.Medium), Calls.Default)
    ),
    (
      Some(Coding -> 0.59),
      Headroom.Ample,
      Allotment(Some(Grade.Standard), Reasoning.Own, Calls.Default)
    ),
    (
      Some(Coding -> 0.6),
      Headroom.Ample,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.High), Doubled)
    ),
    (
      Some(Coding -> 0.59),
      Headroom.Low,
      Allotment(Some(Grade.Standard), Reasoning.AtMost(Effort.Medium), Calls.Default)
    ),
    (
      Some(Coding -> 0.6),
      Headroom.Low,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.Medium), Calls.Default)
    ),
    (
      Some(Decision -> 0.59),
      Headroom.Ample,
      Allotment(Some(Grade.Standard), Reasoning.Own, Calls.Default)
    ),
    (
      Some(Decision -> 0.6),
      Headroom.Ample,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.High), Doubled)
    ),
    (
      Some(Decision -> 0.59),
      Headroom.Low,
      Allotment(Some(Grade.Standard), Reasoning.AtMost(Effort.Medium), Calls.Default)
    ),
    (
      Some(Decision -> 0.6),
      Headroom.Low,
      Allotment(Some(Grade.Strong), Reasoning.At(Effort.Medium), Calls.Default)
    ),
    (None, Headroom.Ample, Allotment(None, Reasoning.Own, Calls.Default)),
    (None, Headroom.Low, Allotment(None, Reasoning.AtMost(Effort.Medium), Calls.Default))
  )

  private def cap(usd: String): Option[DailyCap] =
    Some(DailyCap.of(usd).fold(why => throw new java.lang.AssertionError(why), identity))

  private def spent(cost: Cost): Spend = Spend(3, cost)

  val tests = Tests {
    test("allot gives every input its pinned allotment") {
      val inputs = for {
        asked <- TaskKind.values.toVector.flatMap(k => Vector(Some(k -> 0.59), Some(k -> 0.6))) :+
          None
        headroom <- Headroom.values.toVector
      } yield (asked, headroom)
      table.map((asked, headroom, _) => (asked, headroom)).sortBy(_.toString) ==>
        inputs.sortBy(_.toString)
      val wrong = table.filter { case (asked, headroom, expected) =>
        Router.allot(asked.map(features(_, _)), headroom) != expected
      }
      wrong ==> Vector.empty
    }

    test(
      "headroom is low at 80% of the cap and above, ample below it and with no cap, an unpriced call counting nothing"
    ) {
      Router.headroom(spent(Cost.Exact(BigDecimal("0.80"))), cap("1")) ==> Headroom.Low
      Router.headroom(spent(Cost.Exact(BigDecimal("1.20"))), cap("1")) ==> Headroom.Low
      Router.headroom(spent(Cost.AtLeast(BigDecimal("0.80"))), cap("1")) ==> Headroom.Low
      Router.headroom(spent(Cost.Exact(BigDecimal("0.79"))), cap("1")) ==> Headroom.Ample
      Router.headroom(spent(Cost.AtLeast(BigDecimal("0.79"))), cap("1")) ==> Headroom.Ample
      Router.headroom(spent(Cost.Exact(BigDecimal("50"))), None) ==> Headroom.Ample
    }

    test("the ceiling is standard at low headroom and none at ample") {
      Router.ceiling(Headroom.Low) ==> Some(Grade.Standard)
      Router.ceiling(Headroom.Ample) ==> None
    }
  }
}
