package grit.core.message

import utest.*

object CostTests extends TestSuite {

  private def usage(cost: Option[String]): Usage =
    Usage(Tokens(1), Tokens(1), Tokens.Zero, cost.map(BigDecimal(_)))

  private def shown(usd: String): String = Cost.cents(BigDecimal(usd))

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

    test("an amount under 10¢ keeps two significant digits, trailing zeros dropped") {
      Vector("0.0029", "0.00003", "0.000034567", "0.00194", "0.0012", "0.0000995")
        .map(shown) ==> Vector("0.29¢", "0.003¢", "0.0035¢", "0.19¢", "0.12¢", "0.01¢")
    }

    test("an amount of 10¢ or more keeps its whole cents, the fraction rounded") {
      Vector("0.0124", "0.1256", "1.2345", "1.256", "0.00996", "0.01")
        .map(shown) ==> Vector("1.2¢", "13¢", "123¢", "126¢", "1¢", "1¢")
    }

    test("a half rounds away from zero, either side of 0, a negative with its minus first") {
      Vector("0.00125", "-0.00125", "0.0125").map(shown) ==>
        Vector("0.13¢", "-0.13¢", "1.3¢")
    }

    test("nothing is 0¢, and a tenth of a cent is 0.1¢, a negative with its minus first") {
      Vector("0", "0.000000", "0.001", "-0.001", "-0.0029").map(shown) ==>
        Vector("0¢", "0¢", "0.1¢", "-0.1¢", "-0.29¢")
    }

    test("a cost in cents is its figure when exact, a lower bound with ≥ first") {
      Cost.Exact(BigDecimal("0.0042")).inCents ==> "0.42¢"
      Cost.AtLeast(BigDecimal("0.0124")).inCents ==> "≥ 1.2¢"
      Cost.AtLeast(BigDecimal("0.0029")).inCents ==> "≥ 0.29¢"
    }
  }
}
