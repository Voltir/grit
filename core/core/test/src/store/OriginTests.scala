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
      "a Slack thread's opening is open and its replies focused; a TUI session or task is focused throughout"
    ) {
      val dir = Directory.of("/work").getOrElse(throw new java.lang.AssertionError())
      val origins =
        Vector(Origin.Slack("T1", "C1", "1.0"), Origin.Tui(dir, "s"), Origin.Task("nightly", "1"))
      origins.map(o => (o.focus(Position.Opening), o.focus(Position.Reply))) ==> Vector(
        (Focus.Open, Focus.Focused),
        (Focus.Focused, Focus.Focused),
        (Focus.Focused, Focus.Focused)
      )
      origins.map(_.stitchable) ==> Vector(true, false, false)
    }

    test("only a TUI session is held with the operator") {
      val dir = Directory.of("/work").getOrElse(throw new java.lang.AssertionError())
      Origin.Tui(dir, "s").audience.operator ==> true
      Origin.Slack("T1", "C1", "1.0").audience.operator ==> false
      Origin.Task("nightly", "1").audience.operator ==> false
    }
  }
}
