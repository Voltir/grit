package grit.core.message

import utest.*

object UsageTests extends TestSuite {

  private def usage(tokens: Long, cost: Option[String]): Usage =
    Usage(Tokens(tokens), Tokens(tokens * 2), Tokens(tokens * 3), cost.map(BigDecimal(_)))

  val tests = Tests {
    test("two priced calls cost their sum, and their tokens add") {
      usage(1, Some("0.001")) + usage(10, Some("0.02")) ==> usage(11, Some("0.021"))
    }

    test("a call with no cost makes the sum's cost unknown, not the known part") {
      usage(1, Some("0.001")) + usage(10, None) ==> usage(11, None)
      usage(1, None) + usage(10, Some("0.02")) ==> usage(11, None)
    }

    test("the total of no calls is nothing, at a cost of nothing") {
      Usage.total(Vector.empty) ==> Usage.Zero
      Usage.Zero.costUsd ==> Some(BigDecimal(0))
    }

    test("the total of calls is unknown when any one's cost is") {
      Usage.total(Vector(usage(1, Some("0.001")), usage(2, Some("0.002")))) ==>
        usage(3, Some("0.003"))
      Usage.total(Vector(usage(1, Some("0.001")), usage(2, None), usage(3, Some("0.003")))) ==>
        usage(6, None)
    }
  }
}
