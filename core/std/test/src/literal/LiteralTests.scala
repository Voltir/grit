package grit.std.literal

import utest.*

object LiteralTests extends TestSuite {

  val tests = Tests {
    test("a literal is the value its reader makes of the constant, not the constant") {
      Trimmed.value(Trimmed.literal("  a  ")) ==> "a"
    }
  }
}
