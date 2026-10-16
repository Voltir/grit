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
        "widgets main commit git log recent three commits history repository branchWe need " +
          "just the query line.widgets main commit git log recent three commits history " +
          "repository branch"
      ) ==> "doubled"
      QueryDoubledProbe.doubled(
        "widgets main commits last three git logwidgets main commits last three git log"
      ) ==> "doubled"
      QueryDoubledProbe.doubled(
        "widgets main git commits three lastwidgets main git commits three latest " +
          "recentwidgets main git commits three latest recent"
      ) ==> "doubled"
    }

    test("doubled: a reply that repeats a word but not its opening three is single") {
      QueryDoubledProbe.doubled(
        "widgets main commits last three git log recent three commits widgets repository\u0000"
      ) ==> "single"
    }
  }
}
