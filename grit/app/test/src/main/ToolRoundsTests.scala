package grit.app.main

import utest.*

/** How many model calls a turn may make (`GRIT_TOOL_ROUNDS`). */
object ToolRoundsTests extends TestSuite {

  val tests = Tests {
    test("GRIT_TOOL_ROUNDS: unset is the default; under 2 or not a number is an error") {
      Main.toolRounds(Map.empty).map(_.calls) ==> Right(Main.DefaultToolRounds)
      Main.toolRounds(Map("GRIT_TOOL_ROUNDS" -> " 3 ")).map(_.calls) ==> Right(3)
      assert(Main.toolRounds(Map("GRIT_TOOL_ROUNDS" -> "1")).left.exists(_.contains("at least 2")))
      Main.toolRounds(Map("GRIT_TOOL_ROUNDS" -> "many")) ==>
        Left("GRIT_TOOL_ROUNDS is not a whole number")
    }
  }
}
