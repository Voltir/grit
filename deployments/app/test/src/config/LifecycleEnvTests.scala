package grit.app.config

import scala.concurrent.duration.DurationInt

import grit.core.period.LifecycleSettings
import grit.core.place.Weight

import utest.*

object LifecycleEnvTests extends TestSuite {
  import TestLifecycle.{settings, weight}

  val tests = Tests {
    test("the environment seeds each setting it names; the rest are the defaults") {
      LifecycleEnv.of(Map.empty) ==> Right(LifecycleSettings.Default)
      LifecycleEnv.of(
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
      LifecycleEnv.of(Map("GRIT_IDLE" -> "3")) ==>
        Left("GRIT_IDLE: not a duration: write a whole number and s, m, h or d, as 30s or 3m")
      LifecycleEnv.of(Map("GRIT_BALANCE" -> "4k")) ==> Left(
        "GRIT_BALANCE is not a whole number"
      )
      LifecycleEnv.of(Map("GRIT_BALANCE" -> "0")) ==> Left(
        "GRIT_BALANCE, GRIT_SETTLE, GRIT_RESOLVE_AT, GRIT_ASKS: balance must be at least 1"
      )
      LifecycleEnv.of(Map("GRIT_RESOLVE_AT" -> "80%")) ==>
        Left("GRIT_RESOLVE_AT: not a probability: write a number from 0 to 1, as 0.8")
      LifecycleEnv.of(Map("GRIT_IDLE" -> "30m")) ==> Left(
        "GRIT_BALANCE, GRIT_SETTLE, GRIT_RESOLVE_AT, GRIT_ASKS: settle must be shorter than idle"
      )
    }

    test("the ledger window is declared by GRIT_LEDGER, and never below retention") {
      LifecycleEnv
        .of(Map("GRIT_LEDGER" -> "6m", "GRIT_RETENTION" -> "3m"))
        .map(
          _.windows.ledger
        ) ==> Right(6.minutes)
      LifecycleEnv.of(Map("GRIT_LEDGER" -> "2m", "GRIT_RETENTION" -> "3m")) ==>
        Left("GRIT_IDLE, GRIT_RETENTION, GRIT_LEDGER: ledger must be at least retention")
    }

    test("GRIT_SCOPE set is refused, saying why, whatever it holds") {
      Vector("none", "room", "everywhere").map(s => LifecycleEnv.of(Map("GRIT_SCOPE" -> s))) ==>
        Vector.fill(3)(
          Left(
            "GRIT_SCOPE is no longer read: a window draws on every place its turn may read; unset it"
          )
        )
    }

    test("the environment seeds the weight") {
      LifecycleEnv.of(Map("GRIT_WEIGHT" -> "3")).map(_.weight) ==> Right(weight(3))
      LifecycleEnv.of(Map.empty).map(_.weight) ==> Right(Weight.Default)
      LifecycleEnv.of(Map("GRIT_WEIGHT" -> "0.5")) ==>
        Left("GRIT_WEIGHT: a weight is a number of at least 1, not 0.5")
    }
  }
}
