package grit.core.store

import grit.core.place.Directory

import utest.*

/** [[Origin]]: who its messages are for, and how focused it is. */
object OriginTests extends TestSuite {

  val tests = Tests {
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
