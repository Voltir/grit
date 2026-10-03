package grit.eval.harness.stats

import utest.*

/** A price in mills: two significant digits, never fewer than its whole mills. */
object MillsTests extends TestSuite {

  private def shown(usd: String): String = Mills.of(BigDecimal(usd))

  val tests = Tests {
    test("an amount under 10 mills keeps two significant digits, trailing zeros dropped") {
      Vector("0.0019", "0.00003", "0.000034567", "0.00194", "0.0012", "0.0000995")
        .map(shown) ==> Vector(
        "1.9 mills",
        "0.03 mills",
        "0.035 mills",
        "1.9 mills",
        "1.2 mills",
        "0.1 mills"
      )
    }
    test("an amount of 10 mills or more keeps its whole mills, the fraction rounded") {
      Vector("0.0124", "0.1256", "1.2345", "0.00996")
        .map(shown) ==> Vector("12 mills", "126 mills", "1235 mills", "10 mills")
    }
    test("a half rounds away from zero, either side of 0") {
      Vector("0.00125", "-0.00125", "0.0125").map(shown) ==>
        Vector("1.3 mills", "-1.3 mills", "13 mills")
    }
    test("nothing is 0 mills, and exactly one is a mill") {
      Vector("0", "0.000000", "0.001", "-0.001").map(shown) ==>
        Vector("0 mills", "0 mills", "1 mill", "-1 mill")
    }
    test("a figure is the amount in mills without its unit") {
      Vector("0.0019", "0.001", "0.1256", "0").map(u => Mills.figure(BigDecimal(u))) ==>
        Vector("1.9", "1", "126", "0")
    }
    test("beside its dollars, an amount is exact") {
      Mills.withUsd(BigDecimal("0.00194312")) ==> "1.9 mills ($0.00194312)"
    }
  }
}
