package grit.chat.run

import java.time.Instant

import grit.core.edge.{Console, Holder}

import utest.*

object ChatTests extends TestSuite {

  val tests = Tests {
    test("the status line names who runs the engine") {
      val holder = Holder("m", 7, "e1", Instant.EPOCH, Instant.EPOCH)
      Vector(
        Console.Runner.Here,
        Console.Runner.Elsewhere(None),
        Console.Runner.Elsewhere(Some(holder))
      ).map(Chat.heading("default · ~/grit", _)) ==> Vector(
        "default · ~/grit · engine",
        "default · ~/grit · attached",
        "default · ~/grit · attached · engine on m pid 7"
      )
    }
  }
}
