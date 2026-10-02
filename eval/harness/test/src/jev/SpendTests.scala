package grit.eval.harness.jev

import utest.*

/** A run's cost bounded before it is spent. */
object SpendTests extends TestSuite {

  val tests = Tests {
    // 3,001 bytes is 1,001 tokens, rounded up, and 300 for the call: 1,301 at $0.042 a million.
    test("a request is estimated at its bytes over three, rounded up, and Jev's per-call tokens") {
      Spend.tokens("x" * 3001) ==> 1301L
      Spend.estimate("x" * 3001) ==> BigDecimal("0.000054642")
      // é is two bytes in UTF-8: 6 bytes, 2 tokens, and the call's.
      Spend.tokens("ééé") ==> 302L
    }

    test("a run estimated over its cap is refused before any call") {
      Budget.of(BigDecimal("0.01"), BigDecimal("0.0100001")) ==>
        Left(Budget.Refused(BigDecimal("0.0100001"), BigDecimal("0.01")))
      Budget.of(BigDecimal("0.01"), BigDecimal("0.01")).map(_.spent) ==> Right(BigDecimal(0))
    }

    test("a call that would take what is spent past the cap is not allowed; one up to it is") {
      val budget =
        Budget.of(BigDecimal("0.01"), BigDecimal("0.009")).map(_.spend(BigDecimal("0.008")))
      budget.map(_.allows(BigDecimal("0.002"))) ==> Right(true)
      budget.map(_.allows(BigDecimal("0.0020001"))) ==> Right(false)
    }
  }
}
