package grit.core.period

import java.time.Instant

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.id.{ConversationId, PeriodRef, PeriodSeq, TurnSeq, WorkflowId}

import utest.*

object PeriodTests extends TestSuite {

  private val Noon = Instant.parse("2026-09-20T12:00:00Z")

  private def windows(idle: FiniteDuration, grace: FiniteDuration): Windows =
    Windows.of(idle, grace, 30.days).getOrElse(throw new java.lang.AssertionError(idle))

  private val hourAndFive = windows(1.hour, 5.minutes)

  private val p1 = PeriodRef(ConversationId("c"), PeriodSeq.First)

  val tests = Tests {
    test("with no signal, a period lapses the idle window after its newest activity") {
      Deadline.of(Noon, None, hourAndFive) ==> Due(Noon.plusSeconds(3600), CloseReason.Lapsed)
      Deadline.of(Noon.plusSeconds(600), None, hourAndFive) ==>
        Due(Noon.plusSeconds(4200), CloseReason.Lapsed)
    }

    test("a signal after the newest activity resolves it the grace window after the signal") {
      Deadline.of(Noon, Some(Noon.plusSeconds(60)), hourAndFive) ==>
        Due(Noon.plusSeconds(360), CloseReason.Resolved)
    }

    test("activity after a signal cancels it: the idle window runs from the activity") {
      Deadline.of(Noon.plusSeconds(120), Some(Noon.plusSeconds(60)), hourAndFive) ==>
        Due(Noon.plusSeconds(3720), CloseReason.Lapsed)
      Deadline.of(Noon, Some(Noon), hourAndFive) ==> Due(Noon.plusSeconds(3600), CloseReason.Lapsed)
    }

    test("an open period's deadline is its activity's") {
      Activity(p1, Noon, TurnSeq(0), Some(Noon.plusSeconds(60))).due(hourAndFive) ==>
        Due(Noon.plusSeconds(360), CloseReason.Resolved)
    }

    test("a signal since the newest activity stands; an older one is replaced") {
      val signalled = Activity(p1, Noon, TurnSeq(0), Some(Noon.plusSeconds(60)))
      signalled.signal(Noon.plusSeconds(90)) ==> Noon.plusSeconds(60)
      signalled.copy(newest = Noon.plusSeconds(70)).signal(Noon.plusSeconds(90)) ==>
        Noon.plusSeconds(90)
      signalled.copy(signalled = None).signal(Noon.plusSeconds(90)) ==> Noon.plusSeconds(90)
    }

    test("windows are refused when one is not positive, or the grace outlasts the idle") {
      Windows.of(0.seconds, 1.minute, 1.day) ==> Left("idle must be positive")
      Windows.of(1.hour, 0.seconds, 1.day) ==> Left("grace must be positive")
      Windows.of(1.hour, 1.minute, (-1).days) ==> Left("retention must be positive")
      Windows.of(1.hour, 61.minutes, 1.day) ==> Left("grace must be no longer than idle")
      Windows.of(1.hour, 1.hour, 1.day).map(_.grace) ==> Right(1.hour)
    }

    test("the settings refuse a negative number of closings") {
      LifecycleSettings.of(hourAndFive, -1) ==> Left("closings must not be negative")
      LifecycleSettings.of(hourAndFive, 0).map(_.closings) ==> Right(0)
    }

    test("a purge deletes the turn and every close attempt of each of its turns") {
      Purgeable(p1, TurnSeq(3), TurnSeq(4)).workflows.map(WorkflowId.value) ==>
        Vector("c:3", "c:4", "close:c:1:3", "close:c:1:4")
    }
  }
}
