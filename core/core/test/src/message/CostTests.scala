package grit.core.message

import utest.*

object CostTests extends TestSuite {

  private def usage(cost: Option[String]): Usage =
    Usage(Tokens(1), Tokens(1), Tokens.Zero, cost.map(BigDecimal(_)))

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
  }
}
