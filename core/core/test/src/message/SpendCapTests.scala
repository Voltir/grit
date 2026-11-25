package grit.core.message

import utest.*

object SpendCapTests extends TestSuite {
  val tests = Tests {
    test("a spend cap is a number of dollars above zero: zero and below are refused") {
      SpendCap.of(BigDecimal("0.05")).map(_.usd) ==> Right(BigDecimal("0.05"))
      assert(SpendCap.of(BigDecimal(0)).isLeft)
      assert(SpendCap.of(BigDecimal("-0.01")).isLeft)
    }
    test("a spend cap admits an exact spend below it, never one at or above it or not all priced") {
      SpendCap.of(BigDecimal("0.05")).map { cap =>
        List(
          Cost.Exact(BigDecimal("0.04")),
          Cost.Exact(BigDecimal("0.05")),
          Cost.Exact(BigDecimal("0.06")),
          Cost.AtLeast(BigDecimal(0))
        ).map(cap.admits)
      } ==> Right(List(true, false, false, false))
    }
  }
}
