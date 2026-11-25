package grit.core.spend

import utest.*

object CarveTests extends TestSuite {
  val tests = Tests {
    test("a carve is a number of dollars above zero: zero and below are refused") {
      Carve.of(BigDecimal("0.05")).map(_.usd) ==> Right(BigDecimal("0.05"))
      assert(Carve.of(BigDecimal(0)).isLeft)
      assert(Carve.of(BigDecimal("-0.01")).isLeft)
    }
  }
}
