package grit.core.message

import utest.*

object CostTests extends TestSuite {

  private def usage(cost: Option[String]): Usage =
    Usage(Tokens(1), Tokens(1), Tokens.Zero, cost.map(BigDecimal(_)))

  private def shown(usd: String): String = Cost.mills(BigDecimal(usd))

  val tests = Tests {
    test("calls all priced cost exactly their sum") {
      Cost.total(Vector(usage(Some("0.001")), usage(Some("0.02")))) ==>
        Cost.Exact(BigDecimal("0.021"))
    }

    test("an unpriced call makes the priced calls' sum a lower bound") {
      Cost.total(Vector(usage(Some("0.001")), usage(None), usage(Some("0.02")))) ==>
        Cost.AtLeast(BigDecimal("0.021"))
      Cost.total(Vector(usage(None))) ==> Cost.AtLeast(BigDecimal(0))
    }

    test("a lower bound stays one, whichever side it is on") {
      val exact = Cost.Exact(BigDecimal("0.5"))
      val atLeast = Cost.AtLeast(BigDecimal("0.25"))
      exact + atLeast ==> Cost.AtLeast(BigDecimal("0.75"))
      atLeast + exact ==> Cost.AtLeast(BigDecimal("0.75"))
      atLeast + atLeast ==> Cost.AtLeast(BigDecimal("0.5"))
    }

    test("no calls cost exactly nothing") {
      Cost.total(Vector.empty) ==> Cost.Zero
      Cost.Zero ==> Cost.Exact(BigDecimal(0))
    }

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

    test("a cost in millidollars is its figure when exact, a lower bound with ≥ first") {
      Cost.Exact(BigDecimal("0.0042")).inMills ==> "m$4.2"
      Cost.AtLeast(BigDecimal("0.0124")).inMills ==> "≥ m$12"
    }
  }
}
