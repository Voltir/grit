package grit.app.config

import scala.concurrent.duration.{DurationInt, FiniteDuration}

import grit.core.period.{LifecycleSettings, Probability, Windows}
import grit.core.place.Weight

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
      resolveAt: Double,
      asks: Int,
      weighted: Weight = Weight.Default
  ): LifecycleSettings =
    Windows
      .of(idle, retention, 180.days)
      .flatMap(LifecycleSettings.of(_, balance, settle, p(resolveAt), asks, weighted))
      .getOrElse(throw new java.lang.AssertionError("settings"))

  private def weight(w: Double): Weight = Weight.of(w).fold(e => sys.error(e), identity)

  val tests = Tests {
    test("the environment seeds each setting it names; the rest are the defaults") {
      Lifecycle.fromEnv(Map.empty) ==> Right(LifecycleSettings.Default)
      Lifecycle.fromEnv(
        Map(
          "GRIT_IDLE" -> "3m",
          "GRIT_RETENTION" -> "10m",
          "GRIT_BALANCE" -> "300",
          "GRIT_SETTLE" -> "1m",
          "GRIT_RESOLVE_AT" -> "0.9",
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
        "GRIT_BALANCE, GRIT_SETTLE, GRIT_RESOLVE_AT, GRIT_ASKS: balance must be at least 1"
      )
      Lifecycle.fromEnv(Map("GRIT_RESOLVE_AT" -> "80%")) ==>
        Left("GRIT_RESOLVE_AT: not a probability: write a number from 0 to 1, as 0.8")
      Lifecycle.fromEnv(Map("GRIT_IDLE" -> "30m")) ==> Left(
        "GRIT_BALANCE, GRIT_SETTLE, GRIT_RESOLVE_AT, GRIT_ASKS: settle must be shorter than idle"
      )
    }

    test("/set's changes are a name and a value") {
      Vector(
        "idle 3m",
        " retention 10d ",
        "balance 300",
        "settle 30s",
        "resolve 0.9",
        "asks 1"
      ).map(Change.parse) ==> Vector(
        Right(Change.Idle(3.minutes)),
        Right(Change.Retention(10.days)),
        Right(Change.Balance(300)),
        Right(Change.Settle(30.seconds)),
        Right(Change.ResolveAt(p(0.9))),
        Right(Change.Asks(1))
      )
      Change.parse("idle") ==> Left("idle takes a duration, as 30s or 3m")
      Change.parse("balance -1") ==> Left("balance takes a whole number")
      Change.parse("resolve 2") ==>
        Left("resolve: not a probability: write a number from 0 to 1, as 0.8")
      Change.parse("grace 3m") ==>
        Left(
          "no setting grace: idle, settle, resolve, asks, retention, ledger, balance or weight"
        )
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
      Change.ResolveAt(p(0.95)).applied(now) ==>
        Right(settings(60.minutes, 600.minutes, 4096, 5.minutes, 0.95, 3))
      Change.Settle(2.hours).applied(now) ==> Left("settle must be shorter than idle")
      Change.Idle(5.minutes).applied(now) ==> Left("settle must be shorter than idle")
      Change.Asks(0).applied(now) ==> Left("asks must be at least 1")
    }

    test("the ledger window is declared by GRIT_LEDGER, set by /set, and never below retention") {
      Lifecycle
        .fromEnv(Map("GRIT_LEDGER" -> "6m", "GRIT_RETENTION" -> "3m"))
        .map(
          _.windows.ledger
        ) ==> Right(6.minutes)
      Lifecycle.fromEnv(Map("GRIT_LEDGER" -> "2m", "GRIT_RETENTION" -> "3m")) ==>
        Left("GRIT_IDLE, GRIT_RETENTION, GRIT_LEDGER: ledger must be at least retention")
      Change.parse("ledger 6m") ==> Right(Change.Ledger(6.minutes))
      val now = settings(60.minutes, 600.minutes, 4096, 5.minutes, 0.8, 3)
      Change.Ledger(12.hours).applied(now).map(_.windows.ledger) ==> Right(12.hours)
      Change.Ledger(2.hours).applied(now) ==> Left("ledger must be at least retention")
      Change.Retention(200.days).applied(now) ==> Left("ledger must be at least retention")
    }

    test("GRIT_SCOPE set is refused, saying why, whatever it holds") {
      Vector("none", "room", "everywhere").map(s => Lifecycle.fromEnv(Map("GRIT_SCOPE" -> s))) ==>
        Vector.fill(3)(
          Left(
            "GRIT_SCOPE is no longer read: a window draws on every place its turn may read; unset it"
          )
        )
    }

    test("the environment seeds the weight") {
      Lifecycle.fromEnv(Map("GRIT_WEIGHT" -> "3")).map(_.weight) ==> Right(weight(3))
      Lifecycle.fromEnv(Map.empty).map(_.weight) ==> Right(Weight.Default)
      Lifecycle.fromEnv(Map("GRIT_WEIGHT" -> "0.5")) ==>
        Left("GRIT_WEIGHT: a weight is a number of at least 1, not 0.5")
    }

    test("/set weight changes the weight, every other change keeps it, and scope is no setting") {
      Change.parse("weight 3") ==> Right(Change.Weight(weight(3)))
      Change.parse("weight 0.5") ==> Left("weight: a weight is a number of at least 1, not 0.5")
      Change.parse("scope room") ==>
        Left("no setting scope: idle, settle, resolve, asks, retention, ledger, balance or weight")
      val now = settings(60.minutes, 600.minutes, 4096, 5.minutes, 0.8, 3, weight(3))
      Change.Idle(10.minutes).applied(now).map(_.weight) ==> Right(weight(3))
      Change.Weight(weight(1)).applied(now).map(_.weight) ==> Right(weight(1))
    }

    test("a change /set reports says it holds until grit next starts") {
      val now = settings(3.hours, 1.day, 300, 1.hour, 1.0, 3, weight(3))
      Lifecycle.changed(now) ==>
        "set until grit restarts, when its GRIT_ variables apply again: " + Lifecycle.describe(now)
    }

    test("the settings in one line say where a window draws from") {
      Lifecycle.describe(settings(3.hours, 1.day, 300, 1.hour, 0.8, 3)) ==>
        "after 1h quiet, asks whether anyone is waiting (at most 3 times) and closes when nobody is at 0.8 or " +
        "more; closes after 3h idle; raw entries kept 1d; closings kept 180d after the next; the balance holds 300 bytes; draws on open " +
        "periods and closings it may read, its own weighted 2"
      Lifecycle.describe(settings(3.hours, 1.day, 300, 1.hour, 1.0, 3, weight(1.5))) ==>
        "never asks whether anyone is waiting; closes after 3h idle; raw entries kept 1d; closings kept 180d after the next; " +
        "the balance holds 300 bytes; draws on open periods and closings it may read, its own weighted 1.5"
    }
  }
}
