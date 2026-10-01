package grit.core.spend

import java.time.{Instant, LocalDate, ZoneId}

import grit.core.message.Cost

import utest.*

/** [[Day]], [[DailyCap]] and [[Budget]]: when a day is, and whether a message is taken. */
object SpendTests extends TestSuite {

  private val London = ZoneId.of("Europe/London")

  private def cap(raw: String): DailyCap =
    DailyCap.of(raw).fold(e => throw new java.lang.AssertionError(e), identity)

  val tests = Tests {
    test("the day an instant falls on is the zone's, from its first instant to the next day's") {
      val d = Day.at(Instant.parse("2026-09-28T23:30:00Z"), London) // 00:30 on the 29th in BST
      d ==> Day(LocalDate.of(2026, 9, 29), London)
      d.from ==> Instant.parse("2026-09-28T23:00:00Z")
      d.until ==> Instant.parse("2026-09-29T23:00:00Z")
    }

    test("a day a clock change shortens or lengthens is 23 or 25 hours") {
      val spring = Day(LocalDate.of(2026, 3, 29), London)
      java.time.Duration.between(spring.from, spring.until).toHours ==> 23L
      val autumn = Day(LocalDate.of(2026, 10, 25), London)
      java.time.Duration.between(autumn.from, autumn.until).toHours ==> 25L
    }

    test("a cap is a number of dollars above zero; anything else is refused, saying so") {
      cap("2").usd ==> BigDecimal(2)
      cap(" 0.50 ").usd ==> BigDecimal("0.50")
      for (bad <- Vector("0", "-1", "x", "", "1e400000000000"))
        DailyCap.of(bad) ==> Left(s"a daily cap is a number of dollars above zero, not '$bad'")
    }

    test("with no cap every message is taken; with one, while the priced spend is below it") {
      val none = Budget(London, None)
      none.admits(Spend(9, Cost.Exact(BigDecimal(1000)))) ==> true
      val one = Budget(London, Some(cap("1")))
      one.admits(Spend(1, Cost.Exact(BigDecimal("0.99")))) ==> true
      one.admits(Spend(1, Cost.Exact(BigDecimal("1.00")))) ==> false
      one.admits(Spend(2, Cost.AtLeast(BigDecimal("0.99")))) ==> true
      one.admits(Spend(2, Cost.AtLeast(BigDecimal("1.20")))) ==> false
    }

    test("the refusal names neither a cost nor the cap") {
      (Budget.Refusal.exists(_.isDigit), Budget.Refusal.contains("$")) ==> (false, false)
    }
  }
}
