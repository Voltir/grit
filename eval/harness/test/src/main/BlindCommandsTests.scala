package grit.eval.harness.main

import utest.*

/** [[BlindCommands]]' reading of its `--diff` flags. */
object BlindCommandsTests extends TestSuite {
  val tests = Tests {
    test("--diff pairs are read in order, split at their first =, and the rest are left") {
      val read = BlindCommands.diffFlags(
        List("--task", "t", "--diff", "opus=a=b.patch", "--out", "o", "--diff", "glm=c.patch")
      )
      assert(
        read == Right(
          (Vector("opus" -> "a=b.patch", "glm" -> "c.patch"), List("--task", "t", "--out", "o"))
        )
      )
    }
    test("a --diff with no label, no = or no value is refused naming it") {
      assert(
        BlindCommands.diffFlags(List("--diff", "=a.patch")) == Left(
          "--diff =a.patch: give <label>=<file>"
        )
      )
      assert(
        BlindCommands.diffFlags(List("--diff", "a.patch")) == Left(
          "--diff a.patch: give <label>=<file>"
        )
      )
      assert(BlindCommands.diffFlags(List("--diff")) == Left("--diff takes <label>=<file>"))
    }
  }
}
