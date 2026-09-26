package grit.core.period

import java.time.Instant

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.id.{CloseRef, ConversationId, PeriodRef, PeriodSeq, SettleRef, TurnSeq, WorkflowId}
import grit.core.place.Locality

import utest.*

object PeriodTests extends TestSuite {

  private val Noon = Instant.parse("2026-09-20T12:00:00Z")

  private def p(x: Double): Probability =
    Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  private def settings(
      idle: FiniteDuration = 1.hour,
      settle: FiniteDuration = 5.minutes,
      resolveAt: Double = 0.8,
      asks: Int = 3
  ): LifecycleSettings =
    Windows
      .of(idle, 30.days, 180.days)
      .flatMap(LifecycleSettings.of(_, 3, settle, p(resolveAt), asks, Locality.Default))
      .getOrElse(throw new java.lang.AssertionError(idle))

  private val hourAndFive = settings()

  private val p1 = PeriodRef(ConversationId("c"), PeriodSeq.First)

  /** A verdict at `seconds` after noon about turn `last`, nobody waiting with probability `f`. */
  private def weighed(seconds: Long, last: Long, f: Double): Verdict =
    Verdict(
      Noon.plusSeconds(seconds),
      TurnSeq(last),
      Judgement.Weighed(p(f), p(0), p(1 - f), "jev")
    )

  val tests = Tests {
    test("with no verdict, a period lapses the idle window after its newest activity") {
      Deadline.of(Noon, TurnSeq(0), None, hourAndFive) ==>
        Due(Noon.plusSeconds(3600), CloseReason.Lapsed)
      Deadline.of(Noon.plusSeconds(600), TurnSeq(0), None, hourAndFive) ==>
        Due(Noon.plusSeconds(4200), CloseReason.Lapsed)
    }

    test("a verdict of nobody waiting at the threshold, after the activity, resolves it at once") {
      Deadline.of(Noon, TurnSeq(2), Some(weighed(300, 2, 0.8)), hourAndFive) ==>
        Due(Noon.plusSeconds(300), CloseReason.Resolved(p(0.8)))
    }

    test(
      "a verdict below the threshold, before or at the activity, of another turn, unanswered or with resolveAt 1 leaves the idle deadline"
    ) {
      val lapses = Due(Noon.plusSeconds(3600), CloseReason.Lapsed)
      Vector(
        Deadline.of(Noon, TurnSeq(2), Some(weighed(300, 2, 0.79)), hourAndFive),
        Deadline.of(Noon, TurnSeq(2), Some(weighed(0, 2, 0.99)), hourAndFive),
        Deadline.of(Noon, TurnSeq(3), Some(weighed(300, 2, 0.99)), hourAndFive),
        Deadline.of(
          Noon,
          TurnSeq(2),
          Some(Verdict(Noon.plusSeconds(300), TurnSeq(2), Judgement.Unanswered("down"))),
          hourAndFive
        ),
        Deadline.of(Noon, TurnSeq(2), Some(weighed(300, 2, 1.0)), settings(resolveAt = 1.0))
      ) ==> Vector.fill(5)(lapses)
    }

    test(
      "a quiet period is asked the settle window after its activity, once as it stands, at most asks times, never with resolveAt 1"
    ) {
      Deadline.ask(Noon, TurnSeq(2), None, 0, hourAndFive) ==> Some(Noon.plusSeconds(300))
      Deadline.ask(Noon, TurnSeq(2), Some(weighed(0, 2, 0.1)), 2, hourAndFive) ==>
        Some(Noon.plusSeconds(300))
      Vector(
        Deadline.ask(Noon, TurnSeq(2), Some(weighed(300, 2, 0.1)), 1, hourAndFive),
        Deadline.ask(Noon, TurnSeq(2), None, 3, hourAndFive),
        Deadline.ask(Noon, TurnSeq(2), None, 0, settings(resolveAt = 1.0))
      ) ==> Vector(None, None, None)
    }

    test("an open period's deadline, attempt and question are its activity's") {
      val a = Activity(p1, Noon, TurnSeq(2), Some(weighed(400, 2, 0.9)), 1)
      a.due(hourAndFive) ==> Due(Noon.plusSeconds(400), CloseReason.Resolved(p(0.9)))
      a.attempt(hourAndFive) ==> CloseRef(p1, TurnSeq(2), Noon.plusSeconds(400))
      a.copy(verdict = None).attempt(settings(idle = 2.hours)) ==>
        CloseRef(p1, TurnSeq(2), Noon.plusSeconds(7200))
      a.question ==> SettleRef(p1, TurnSeq(2), Noon)
      a.copy(verdict = None).asks(hourAndFive) ==> Some(Noon.plusSeconds(300))
    }

    test("windows are refused when one is not positive") {
      Windows.of(0.seconds, 1.day, 1.day) ==> Left("idle must be positive")
      Windows.of(1.hour, (-1).days, 1.day) ==> Left("retention must be positive")
    }

    test("a ledger window shorter than retention is refused; one as long is kept") {
      Windows.of(1.hour, 2.days, 1.day) ==> Left("ledger must be at least retention")
      Windows.of(1.hour, 2.days, 2.days).map(_.ledger) ==> Right(2.days)
    }

    test(
      "the settings refuse a balance below 1, a settle not shorter than idle, resolveAt 0 and no asks"
    ) {
      val w = Windows.of(1.hour, 1.day, 1.day).getOrElse(throw new java.lang.AssertionError("w"))
      Vector(
        LifecycleSettings.of(w, 0, 5.minutes, p(0.8), 3, Locality.Default),
        LifecycleSettings.of(w, 1, 0.seconds, p(0.8), 3, Locality.Default),
        LifecycleSettings.of(w, 1, 1.hour, p(0.8), 3, Locality.Default),
        LifecycleSettings.of(w, 1, 5.minutes, p(0), 3, Locality.Default),
        LifecycleSettings.of(w, 1, 5.minutes, p(0.8), 0, Locality.Default)
      ) ==> Vector(
        Left("balance must be at least 1"),
        Left("settle must be positive"),
        Left("settle must be shorter than idle"),
        Left("resolveAt must be above 0"),
        Left("asks must be at least 1")
      )
      LifecycleSettings
        .of(w, 1, 59.minutes, p(1.0), 1, Locality.Default)
        .map(s => (s.balance, s.settle, s.asks)) ==>
        Right((1, 59.minutes, 1))
    }

    test("a probability is between 0 and 1, both included") {
      Vector(-0.01, 0.0, 1.0, 1.01, Double.NaN).map(Probability.of(_).map(Probability.value)) ==>
        Vector(None, Some(0.0), Some(1.0), None, None)
    }

    test(
      "a purge deletes each of its turns, and every close attempt and question by its id's prefix"
    ) {
      val purgeable = Purgeable(p1, TurnSeq(3), TurnSeq(4))
      (purgeable.turns.map(WorkflowId.value), purgeable.attempts) ==>
        (Vector("c:3", "c:4"), Vector("close:c:1:", "settle:c:1:"))
    }
  }
}
