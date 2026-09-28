package grit.app.config

import java.time.ZoneId

import grit.core.spend.{Budget, DailyCap}

import utest.*

/** [[Budgets]]: the daily cap as a person sets it. */
object BudgetsTests extends TestSuite {

  private val zone = ZoneId.of("America/Los_Angeles")

  private def cap(raw: String): Option[DailyCap] = DailyCap.of(raw).toOption

  val tests = Tests {
    test("GRIT_DAILY_USD sets the cap; unset, the default holds; serve's default is $1.00") {
      Budgets.fromEnv(Map("GRIT_DAILY_USD" -> "2.50"), zone, None) ==>
        Right(Budget(zone, cap("2.50")))
      Budgets.fromEnv(Map.empty, zone, cap("3")) ==> Right(Budget(zone, cap("3")))
      Budgets.fromEnv(Map.empty, zone, None) ==> Right(Budget(zone, None))
      Budgets.ServeDefault.map(_.usd) ==> Some(BigDecimal("1.00"))
    }

    test("a cap that is not dollars above zero is refused, naming the variable") {
      Budgets.fromEnv(Map("GRIT_DAILY_USD" -> "0"), zone, Budgets.ServeDefault) ==>
        Left("GRIT_DAILY_USD: a daily cap is a number of dollars above zero, not '0'")
    }
  }
}
