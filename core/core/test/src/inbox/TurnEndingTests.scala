package grit.core.inbox

import utest.*

object TurnEndingTests extends TestSuite {

  val tests = Tests {
    test("a reply is answered, a reply at its limit is at its limit, any other outcome neither") {
      Vector(
        "replied: r1; summarised: s1",
        "replied: r1; summarised: s1" + TurnEnding.AtLimitSaid,
        "replied: r1; no summary: down",
        "posted: d1; summarised: s1",
        "drafted: d1; not settled: gone",
        "failed: the provider refused"
      ).map(TurnEnding.of) ==> Vector(
        TurnEnding.Answered,
        TurnEnding.AtLimit,
        TurnEnding.Answered,
        TurnEnding.Otherwise,
        TurnEnding.Otherwise,
        TurnEnding.Otherwise
      )
    }
  }
}
