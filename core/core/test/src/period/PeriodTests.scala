package grit.core.period

import java.time.Instant

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.id.{
  CloseRef,
  ConversationId,
  PeriodRef,
  PeriodSeq,
  SettleRef,
  StitchRef,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.place.Weight

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
      .flatMap(LifecycleSettings.of(_, 3, settle, p(resolveAt), asks, Weight.Default))
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

  /** Settings whose rolling closes at `chars` characters, or a day after opening. */
  private def budget(chars: Int): LifecycleSettings =
    (for {
      r <- Rolling.of(chars, 1.day, grit.core.message.Tokens(100))
      w <- Windows.of(1.hour, 30.days, 180.days)
      s <- LifecycleSettings.of(w, 3, 5.minutes, p(0.8), 3, Weight.Default, r)
    } yield s).getOrElse(throw new java.lang.AssertionError(chars))

  /** An open period of [[p1]] kept at `pace`, newest active at noon, its newest turn `last`,
    * and a verdict of nobody waiting about it, which no rolling period heeds.
    */
  private def rolling(pace: Pace, last: Long): Activity =
    Activity(p1, Noon, TurnSeq(last), Some(weighed(30, last, 0.99)), 0, pace)

  /** An open period of [[p1]] that settles, newest active at `newest`, its newest turn `last`. */
  private def settling(
      newest: Instant,
      last: Long,
      verdict: Option[Verdict] = None,
      asked: Int = 0
  ): Activity =
    Activity(p1, newest, TurnSeq(last), verdict, asked, Pace.Settles)

  val tests = Tests {
    test("with no verdict, a period lapses the idle window after its newest activity") {
      Deadline.of(settling(Noon, 0), hourAndFive) ==>
        Due(Noon.plusSeconds(3600), CloseReason.Lapsed, TurnSeq(0))
      Deadline.of(settling(Noon.plusSeconds(600), 0), hourAndFive) ==>
        Due(Noon.plusSeconds(4200), CloseReason.Lapsed, TurnSeq(0))
    }

    test("a verdict of nobody waiting at the threshold, after the activity, resolves it at once") {
      Deadline.of(settling(Noon, 2, Some(weighed(300, 2, 0.8))), hourAndFive) ==>
        Due(Noon.plusSeconds(300), CloseReason.Resolved(p(0.8)), TurnSeq(2))
    }

    test(
      "a verdict below the threshold, before or at the activity, of another turn, unanswered or with resolveAt 1 leaves the idle deadline"
    ) {
      val lapses = Due(Noon.plusSeconds(3600), CloseReason.Lapsed, TurnSeq(2))
      Vector(
        Deadline.of(settling(Noon, 2, Some(weighed(300, 2, 0.79))), hourAndFive),
        Deadline.of(settling(Noon, 2, Some(weighed(0, 2, 0.99))), hourAndFive),
        Deadline.of(settling(Noon, 2, Some(weighed(300, 1, 0.99))), hourAndFive),
        Deadline.of(
          settling(
            Noon,
            2,
            Some(Verdict(Noon.plusSeconds(300), TurnSeq(2), Judgement.Unanswered("down")))
          ),
          hourAndFive
        ),
        Deadline.of(settling(Noon, 2, Some(weighed(300, 2, 1.0))), settings(resolveAt = 1.0))
      ) ==> Vector.fill(5)(lapses)
    }

    test(
      "a quiet period is asked the settle window after its activity, once as it stands, at most asks times, never with resolveAt 1"
    ) {
      Deadline.ask(settling(Noon, 2), hourAndFive) ==> Some(Noon.plusSeconds(300))
      Deadline.ask(settling(Noon, 2, Some(weighed(0, 2, 0.1)), 2), hourAndFive) ==>
        Some(Noon.plusSeconds(300))
      Vector(
        Deadline.ask(settling(Noon, 2, Some(weighed(300, 2, 0.1)), 1), hourAndFive),
        Deadline.ask(settling(Noon, 2, None, 3), hourAndFive),
        Deadline.ask(settling(Noon, 2), settings(resolveAt = 1.0))
      ) ==> Vector(None, None, None)
    }

    test("an open period's deadline, attempt and question are its activity's") {
      val a = settling(Noon, 2, Some(weighed(400, 2, 0.9)), 1)
      a.due(hourAndFive) ==> Due(Noon.plusSeconds(400), CloseReason.Resolved(p(0.9)), TurnSeq(2))
      a.attempt(hourAndFive) ==> CloseRef(p1, TurnSeq(2), Noon.plusSeconds(400))
      a.copy(verdict = None).attempt(settings(idle = 2.hours)) ==>
        CloseRef(p1, TurnSeq(2), Noon.plusSeconds(7200))
      a.question ==> SettleRef(p1, TurnSeq(2), Noon)
      a.copy(verdict = None).asks(hourAndFive) ==> Some(Noon.plusSeconds(300))
    }

    test(
      "a rolling period is due when the turn that brought what was said to the budget began, cut at that turn, whatever came after"
    ) {
      // Turns 1 to 4, a minute apart; 4,000 characters reached at turn 3.
      val spoken = Vector(1500L, 1500L, 1000L, 2000L).zipWithIndex.map((c, i) =>
        Spoken(TurnSeq(i + 1L), Noon.plusSeconds(60L * (i + 1)), c)
      )
      val a = rolling(Pace.Rolls(Noon, spoken), last = 4)
      a.due(budget(4000)) ==> Due(Noon.plusSeconds(180), CloseReason.Rolled, TurnSeq(3))
      a.attempt(budget(4000)) ==> CloseRef(p1, TurnSeq(3), Noon.plusSeconds(180))
      // Under the budget by one character, it is not due until its most.
      a.due(budget(6001)) ==> Due(Noon.plusSeconds(86400), CloseReason.Rolled, TurnSeq(4))
    }

    test(
      "a rolling period under its budget is due its most after it opened, at the newest turn begun by then; never asked, never lapsed"
    ) {
      val spoken = Vector(
        Spoken(TurnSeq(1), Noon, 10),
        Spoken(TurnSeq(2), Noon.plusSeconds(3600), 10),
        Spoken(TurnSeq(3), Noon.plusSeconds(86401), 10)
      )
      // Quiet for a day before turn 3: a period that settled would have lapsed.
      val a = rolling(Pace.Rolls(Noon, spoken), last = 3)
      a.due(budget(4000)) ==> Due(Noon.plusSeconds(86400), CloseReason.Rolled, TurnSeq(2))
      a.asks(budget(4000)) ==> None
      rolling(Pace.Rolls(Noon, Vector.empty), last = 1).due(budget(4000)) ==>
        Due(Noon.plusSeconds(86400), CloseReason.Rolled, TurnSeq(1))
    }

    test("a rolling is refused under one character, with no most, or shown below nothing") {
      val t = grit.core.message.Tokens(1)
      Vector(
        Rolling.of(0, 1.day, t),
        Rolling.of(1, 0.seconds, t),
        Rolling.of(1, 1.day, grit.core.message.Tokens(-1))
      ) ==> Vector(
        Left("chars must be at least 1"),
        Left("most must be positive"),
        Left("shown must not be negative")
      )
      Rolling.of(1, 1.day, grit.core.message.Tokens.Zero).isRight ==> true
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
        LifecycleSettings.of(w, 0, 5.minutes, p(0.8), 3, Weight.Default),
        LifecycleSettings.of(w, 1, 0.seconds, p(0.8), 3, Weight.Default),
        LifecycleSettings.of(w, 1, 1.hour, p(0.8), 3, Weight.Default),
        LifecycleSettings.of(w, 1, 5.minutes, p(0), 3, Weight.Default),
        LifecycleSettings.of(w, 1, 5.minutes, p(0.8), 0, Weight.Default)
      ) ==> Vector(
        Left("balance must be at least 1"),
        Left("settle must be positive"),
        Left("settle must be shorter than idle"),
        Left("resolveAt must be above 0"),
        Left("asks must be at least 1")
      )
      LifecycleSettings
        .of(w, 1, 59.minutes, p(1.0), 1, Weight.Default)
        .map(s => (s.balance, s.settle, s.asks)) ==>
        Right((1, 59.minutes, 1))
    }

    test("a probability is between 0 and 1, both included") {
      Vector(-0.01, 0.0, 1.0, 1.01, Double.NaN).map(Probability.of(_).map(Probability.value)) ==>
        Vector(None, Some(0.0), Some(1.0), None, None)
    }

    test(
      "a purge deletes each of its turns, and every close attempt, question, triage and shadow by its id's prefix, and placements by theirs"
    ) {
      val purgeable = Purgeable(p1, TurnSeq(3), TurnSeq(4))
      (purgeable.turns.map(WorkflowId.value), purgeable.attempts) ==>
        (
          Vector("c:3", "c:4"),
          Vector("close:c:1:", "settle:c:1:", "triage:c:1:", "shadow:c:1:", "stitch:")
        )
      purgeable.holds(WorkflowId("triage:c:1:4")) ==> true
    }

    test("of the placements, a purge deletes those of its own turns, and no other's") {
      val purgeable = Purgeable(p1, TurnSeq(3), TurnSeq(5))
      def placement(in: ConversationId, turn: Long): WorkflowId =
        StitchRef(TurnRef(in, TurnSeq(turn)), Instant.EPOCH).workflowId
      Vector(3L, 5L, 2L, 6L).map(t => purgeable.holds(placement(p1.conversationId, t))) ==>
        Vector(true, true, false, false)
      purgeable.holds(placement(ConversationId("d"), 4)) ==> false
    }
  }
}
