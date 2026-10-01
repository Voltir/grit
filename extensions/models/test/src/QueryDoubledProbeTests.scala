package grit.models

import utest.*

/** What the probe counts as a doubled reply, over replies gpt-oss-120b on cerebras/fp16 gave
  * the query role on 2026-10-01.
  */
object QueryDoubledProbeTests extends TestSuite {

  val tests = Tests {
    test("doubled: a reply whose first three words recur, joined without a space or not") {
      // Two answers with reasoning text between them, then two run together, then three.
      QueryDoubledProbe.doubled(
        "actualbest main commit git log recent three commits history repository branchWe need " +
          "just the query line.actualbest main commit git log recent three commits history " +
          "repository branch"
      ) ==> "doubled"
      QueryDoubledProbe.doubled(
        "actualbest main commits last three git logactualbest main commits last three git log"
      ) ==> "doubled"
      QueryDoubledProbe.doubled(
        "actualbest main git commits three lastactualbest main git commits three latest " +
          "recentactualbest main git commits three latest recent"
      ) ==> "doubled"
    }

    test("doubled: a reply that repeats a word but not its opening three is single") {
      QueryDoubledProbe.doubled(
        "actualbest main commits last three git log recent three commits actualbest repository\u0000"
      ) ==> "single"
    }
  }
}
