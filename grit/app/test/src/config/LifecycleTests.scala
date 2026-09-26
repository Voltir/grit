package grit.app.config

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.period.{LifecycleSettings, Probability, Windows}

import utest.*

object LifecycleTests extends TestSuite {
  import Lifecycle.Change

  private def p(x: Double): Probability =
    Probability.of(x).getOrElse(throw new java.lang.AssertionError(x))

  private def settings(
      idle: FiniteDuration,
      retention: FiniteDuration,
      balance: Int,
      settle: FiniteDuration,
      finishedAt: Double,
      asks: Int
  ): LifecycleSettings =
    Windows
      .of(idle, retention)
      .flatMap(LifecycleSettings.of(_, balance, settle, p(finishedAt), asks))
      .getOrElse(throw new java.lang.AssertionError("settings"))

  val tests = Tests {
    test("the environment seeds each setting it names; the rest are the defaults") {
      Lifecycle.fromEnv(Map.empty) ==> Right(LifecycleSettings.Default)
      Lifecycle.fromEnv(
        Map(
          "GRIT_IDLE" -> "3m",
          "GRIT_RETENTION" -> "10m",
          "GRIT_BALANCE" -> "300",
          "GRIT_SETTLE" -> "1m",
          "GRIT_FINISHED_AT" -> "0.9",
          "GRIT_ASKS" -> "2"
        )
      ) ==> Right(settings(3.minutes, 10.minutes, 300, 1.minute, 0.9, 2))
    }

    test("a setting the environment writes badly, or settings that break the rules, say why") {
      Lifecycle.fromEnv(Map("GRIT_IDLE" -> "3")) ==>
        Left("GRIT_IDLE: not a duration: write a whole number and s, m, h or d, as 30s or 3m")
      Lifecycle.fromEnv(Map("GRIT_BALANCE" -> "4k")) ==> Left(
        "GRIT_BALANCE is not a whole number"
      )
      Lifecycle.fromEnv(Map("GRIT_BALANCE" -> "0")) ==> Left(
        "GRIT_BALANCE, GRIT_SETTLE, GRIT_FINISHED_AT, GRIT_ASKS: balance must be at least 1"
      )
      Lifecycle.fromEnv(Map("GRIT_FINISHED_AT" -> "80%")) ==>
        Left("GRIT_FINISHED_AT: not a probability: write a number from 0 to 1, as 0.8")
      Lifecycle.fromEnv(Map("GRIT_IDLE" -> "30m")) ==> Left(
        "GRIT_BALANCE, GRIT_SETTLE, GRIT_FINISHED_AT, GRIT_ASKS: settle must be shorter than idle"
      )
    }

    test("/set's changes are a name and a value") {
      Vector(
        "idle 3m",
        " retention 10d ",
        "balance 300",
        "settle 30s",
        "finished 0.9",
        "asks 1"
      ).map(Change.parse) ==> Vector(
        Right(Change.Idle(3.minutes)),
        Right(Change.Retention(10.days)),
        Right(Change.Balance(300)),
        Right(Change.Settle(30.seconds)),
        Right(Change.FinishedAt(p(0.9))),
        Right(Change.Asks(1))
      )
      Change.parse("idle") ==> Left("idle takes a duration, as 30s or 3m")
      Change.parse("balance -1") ==> Left("balance takes a whole number")
      Change.parse("finished 2") ==>
        Left("finished: not a probability: write a number from 0 to 1, as 0.8")
      Change.parse("grace 3m") ==>
        Left("no setting grace: idle, settle, finished, asks, retention or balance")
      Change.parse("idle 3") ==>
        Left("idle: not a duration: write a whole number and s, m, h or d, as 30s or 3m")
    }

    test("a change applies to one setting, and is refused when the result breaks the rules") {
      val now = settings(60.minutes, 600.minutes, 4096, 5.minutes, 0.8, 3)
      Change.Idle(10.minutes).applied(now) ==>
        Right(settings(10.minutes, 600.minutes, 4096, 5.minutes, 0.8, 3))
      Change.Balance(300).applied(now) ==>
        Right(settings(60.minutes, 600.minutes, 300, 5.minutes, 0.8, 3))
      Change.Balance(0).applied(now) ==> Left("balance must be at least 1")
      Change.FinishedAt(p(0.95)).applied(now) ==>
        Right(settings(60.minutes, 600.minutes, 4096, 5.minutes, 0.95, 3))
      Change.Settle(2.hours).applied(now) ==> Left("settle must be shorter than idle")
      Change.Idle(5.minutes).applied(now) ==> Left("settle must be shorter than idle")
      Change.Asks(0).applied(now) ==> Left("asks must be at least 1")
    }

    test("the settings in one line; at finished 1, nothing is asked") {
      Lifecycle.describe(settings(3.hours, 1.day, 300, 1.hour, 0.8, 3)) ==>
        "after 1h quiet, asks whether it is finished (at most 3 times) and closes at 0.8 or " +
        "more; closes after 3h idle; raw entries kept 1d; the balance holds 300 bytes"
      Lifecycle.describe(settings(3.hours, 1.day, 300, 1.hour, 1.0, 3)) ==>
        "never asks whether it is finished; closes after 3h idle; raw entries kept 1d; " +
        "the balance holds 300 bytes"
    }
  }
}
