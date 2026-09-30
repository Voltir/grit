package grit.kit.run

import grit.core.edge.Unheard

import utest.*

/** [[Estimate.of]]: the bound printed before a backfill hears anything. */
object EstimateTests extends TestSuite {

  val tests = Tests {
    test(
      "each message is triaged with the thread before it, cut to its last 2000 characters, and each thread written a closing"
    ) {
      val e = Estimate.of(Unheard("#standup", Vector(Vector(100, 50), Vector(10), Vector(3000, 2))))
      // Characters: 2100, 2010, 2150 (100 before it), 5000, 4002 (2000 of 3000 before it):
      // 15262, 3815.5 tokens at $0.042 a million.
      e ==> Estimate(5, 3, BigDecimal("0.000160251"), BigDecimal("0.0045"))
      e.total ==> BigDecimal("0.004660251")
    }
  }
}
