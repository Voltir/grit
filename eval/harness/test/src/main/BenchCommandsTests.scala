package grit.eval.harness.main

import utest.*

/** [[BenchCommands]]' reading of its flags. */
object BenchCommandsTests extends TestSuite {

  val tests = Tests {

    test(
      "caps are read from their flags, rounds from --max-rounds; --max-turns is refused naming it"
    ) {
      val flags = Map("model" -> "claude-haiku-5-5", "max-budget-usd" -> "0.05")
      (
        BenchCommands.caps(flags.updated("max-rounds", "6")).map(_.maxRounds),
        BenchCommands.caps(flags.updated("max-turns", "6")),
        BenchCommands.caps(flags.updated("max-rounds", "6").updated("max-turns", "6")),
        BenchCommands.caps(flags)
      ) ==> (
        Right(6),
        Left("--max-turns is now --max-rounds, the rounds of a turn"),
        Left("--max-turns is now --max-rounds, the rounds of a turn"),
        Left("--max-rounds is required")
      )
    }
  }
}
