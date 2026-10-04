package grit.eval.harness.stats

import utest.*

/** A price in millidollars, written m$: two significant digits, never fewer than its whole
  * millidollars.
  */
object MillsTests extends TestSuite {

  private def shown(usd: String): String = Mills.figure(BigDecimal(usd))

  val tests = Tests {
    test("an amount under m$10 keeps two significant digits, trailing zeros dropped") {
      Vector("0.0029", "0.00003", "0.000034567", "0.00194", "0.0012", "0.0000995")
        .map(shown) ==> Vector("m$2.9", "m$0.03", "m$0.035", "m$1.9", "m$1.2", "m$0.1")
    }
    test("an amount of m$10 or more keeps its whole millidollars, the fraction rounded") {
      Vector("0.0124", "0.1256", "1.2345", "0.00996")
        .map(shown) ==> Vector("m$12", "m$126", "m$1235", "m$10")
    }
    test("a half rounds away from zero, either side of 0, a negative with its minus first") {
      Vector("0.00125", "-0.00125", "0.0125").map(shown) ==>
        Vector("m$1.3", "-m$1.3", "m$13")
    }
    test("nothing is m$0, and one millidollar m$1, singular or plural alike") {
      Vector("0", "0.000000", "0.001", "-0.001").map(shown) ==>
        Vector("m$0", "m$0", "m$1", "-m$1")
    }
  }
}
