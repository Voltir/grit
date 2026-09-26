package grit.app.config

import scala.concurrent.duration.DurationInt

import grit.core.period.{LifecycleSettings, Windows}

import utest.*

object LifecycleTests extends TestSuite {
  import Lifecycle.Change

  private def settings(idle: Int, grace: Int, retention: Int, closings: Int): LifecycleSettings =
    Windows
      .of(idle.minutes, grace.minutes, retention.minutes)
      .flatMap(LifecycleSettings.of(_, closings))
      .getOrElse(throw new java.lang.AssertionError("settings"))

  val tests = Tests {
    test("the environment seeds each setting it names; the rest are the defaults") {
      Lifecycle.fromEnv(Map.empty) ==> Right(LifecycleSettings.Default)
      Lifecycle.fromEnv(
        Map(
          "GRIT_IDLE" -> "3m",
          "GRIT_GRACE" -> "1m",
          "GRIT_RETENTION" -> "10m",
          "GRIT_WINDOW_K" -> "2"
        )
      ) ==> Right(settings(3, 1, 10, 2))
      Lifecycle.fromEnv(Map("GRIT_WINDOW_K" -> "0")).map(_.closings) ==> Right(0)
    }

    test("a setting the environment writes badly, or settings that break the rules, say why") {
      Lifecycle.fromEnv(Map("GRIT_IDLE" -> "3")) ==>
        Left("GRIT_IDLE: not a duration: write a whole number and s, m, h or d, as 30s or 3m")
      Lifecycle.fromEnv(Map("GRIT_WINDOW_K" -> "two")) ==> Left(
        "GRIT_WINDOW_K is not a whole number"
      )
      Lifecycle.fromEnv(Map("GRIT_IDLE" -> "10m")) ==>
        Left("GRIT_IDLE, GRIT_GRACE, GRIT_RETENTION: grace must be no longer than idle")
    }

    test("/set's changes are a name and a value") {
      Vector("idle 3m", "grace 30s", " retention 10d ", "closings 2").map(Change.parse) ==> Vector(
        Right(Change.Idle(3.minutes)),
        Right(Change.Grace(30.seconds)),
        Right(Change.Retention(10.days)),
        Right(Change.Closings(2))
      )
      Change.parse("idle") ==> Left("idle takes a duration, as 30s or 3m")
      Change.parse("closings -1") ==> Left("closings takes a whole number")
      Change.parse("budget 3") ==> Left("no setting budget: idle, grace, retention or closings")
      Change.parse("idle 3") ==>
        Left("idle: not a duration: write a whole number and s, m, h or d, as 30s or 3m")
    }

    test("a change applies to one setting, and is refused when the result breaks the rules") {
      val now = settings(60, 5, 600, 3)
      Change.Idle(10.minutes).applied(now) ==> Right(settings(10, 5, 600, 3))
      Change.Closings(0).applied(now) ==> Right(settings(60, 5, 600, 0))
      Change.Grace(2.hours).applied(now) ==> Left("grace must be no longer than idle")
    }

    test("the settings in one line") {
      Lifecycle.describe(settings(3, 1, 60 * 24, 2)) ==>
        "closes after 3m idle, or 1m after /done; raw entries kept 1d; 2 closings open a window"
    }
  }
}
