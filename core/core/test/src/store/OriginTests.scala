package grit.core.store

import grit.core.id.PrincipalId
import grit.core.place.Directory

import utest.*

/** [[Origin]]: who the assistant is where a conversation happens, and who its messages are for. */
object OriginTests extends TestSuite {

  val tests = Tests {
    test(
      "a Slack thread's assistant is its team's, slack:{team}; a TUI session's and a task's is none"
    ) {
      Origin.Slack("T1", "C1", "1.0").assistant ==> Some(PrincipalId("slack:T1"))
      Origin.Slack("T1", "C2", "2.0").assistant ==> Some(PrincipalId("slack:T1"))
      val dir = Directory.of("/work").getOrElse(throw new java.lang.AssertionError())
      Origin.Tui(dir, "s").assistant ==> None
      Origin.Task("nightly", "1").assistant ==> None
    }

    test(
      "a Slack thread's first message is stitchable; a TUI session's and a task's are not"
    ) {
      val dir = Directory.of("/work").getOrElse(throw new java.lang.AssertionError())
      Origin.Slack("T1", "C1", "1.0").audience.stitchable ==> true
      Origin.Tui(dir, "s").audience.stitchable ==> false
      Origin.Task("nightly", "1").audience.stitchable ==> false
    }
    test("only a TUI session is held with the operator") {
      val dir = Directory.of("/work").getOrElse(throw new java.lang.AssertionError())
      Origin.Tui(dir, "s").audience.operator ==> true
      Origin.Slack("T1", "C1", "1.0").audience.operator ==> false
      Origin.Task("nightly", "1").audience.operator ==> false
    }
  }
}
