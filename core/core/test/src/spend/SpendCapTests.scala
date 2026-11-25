package grit.core.spend

import utest.*

object SpendCapTests extends TestSuite {
  val tests = Tests {
    test("a spend cap is a number of dollars above zero: zero and below are refused") {
      SpendCap.of(BigDecimal("0.05")).map(_.usd) ==> Right(BigDecimal("0.05"))
      assert(SpendCap.of(BigDecimal(0)).isLeft)
      assert(SpendCap.of(BigDecimal("-0.01")).isLeft)
    }
  }
}
