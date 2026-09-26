package grit.app.main

import grit.app.look.Look

import utest.*

/** How many model calls a turn may make (`GRIT_TOOL_ROUNDS`), and how a loop's steps are
  * named in the status line.
  */
object ToolRoundsTests extends TestSuite {

  val tests = Tests {
    test("GRIT_TOOL_ROUNDS: unset is the default; under 2 or not a number is an error") {
      Main.toolRounds(Map.empty).map(_.calls) ==> Right(Main.DefaultToolRounds)
      Main.toolRounds(Map("GRIT_TOOL_ROUNDS" -> " 3 ")).map(_.calls) ==> Right(3)
      assert(Main.toolRounds(Map("GRIT_TOOL_ROUNDS" -> "1")).left.exists(_.contains("at least 2")))
      Main.toolRounds(Map("GRIT_TOOL_ROUNDS" -> "many")) ==>
        Left("GRIT_TOOL_ROUNDS is not a whole number")
    }

    test("GRIT_TOOLS: unset is every tool in the chat and the read-only ones in a run") {
      Main.toolChoice(Map.empty, chat = true) ==> Right(Main.ToolChoice.All)
      Main.toolChoice(Map.empty, chat = false) ==> Right(Main.ToolChoice.Read)
      for (chat <- Seq(true, false)) {
        Main.toolChoice(Map("GRIT_TOOLS" -> "read"), chat) ==> Right(Main.ToolChoice.Read)
        Main.toolChoice(Map("GRIT_TOOLS" -> "write"), chat) ==>
          Left("GRIT_TOOLS is neither read nor all")
      }
      Main.toolChoice(Map("GRIT_TOOLS" -> " all "), chat = true) ==> Right(Main.ToolChoice.All)
      // A run has nobody to approve a call that changes something.
      Main.toolChoice(Map("GRIT_TOOLS" -> "all"), chat = false) ==>
        Left("GRIT_TOOLS=all needs the chat, which answers what a tool asks first")
    }

    test("a loop's steps show by their family") {
      Vector("call-model:2", "record-call:0", "ask:1:3", "tool:1:3").map(Look.Runes.step) ==>
        Vector("ᚨ answering", "ᛃ recording", "ᛏ asking you", "ᛏ using a tool")
    }
  }
}
