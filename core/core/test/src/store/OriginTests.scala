package grit.core.store

import grit.core.id.EdgeName
import grit.core.identity.TestAccounts
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

    test(
      "a room is its channel's top level, every message open; a thread under it is at its root's place, focused; only a legacy thread is stitched, and only a room rolls"
    ) {
      val room = Origin.Room("T1", "C1")
      val thread = Origin.Thread("T1", "C1", "1.0")
      val legacy = Origin.Slack("T1", "C1", "1.0")
      val origins = Vector(room, thread, legacy)
      origins.map(o => (o.place.written, o.room.written)) ==> Vector(
        ("slack:T1/C1", "slack:T1/C1"),
        ("slack:T1/C1/1.0", "slack:T1/C1"),
        ("slack:T1/C1/1.0", "slack:T1/C1")
      )
      origins.map(o => (o.focus(Position.Opening), o.focus(Position.Reply))) ==> Vector(
        (Focus.Open, Focus.Open),
        (Focus.Focused, Focus.Focused),
        (Focus.Open, Focus.Focused)
      )
      origins.map(o => (o.stitchable, o.rolls)) ==> Vector(
        (false, true),
        (false, false),
        (true, false)
      )
      origins.map(_.edge).distinct ==> Vector(EdgeName.Slack)
    }

    test(
      "a direct message's room is its account's under direct, its thread below it; its edge is its account's namespace's, its audience one person, and it is never stitched"
    ) {
      val dm = Origin.Direct(TestAccounts.sourced("slack:T/U"), "1712.3")
      (
        dm.room.written,
        dm.place.written,
        dm.edge,
        dm.audience,
        dm.audience.operator,
        dm.focus(Position.Opening),
        dm.stitchable
      ) ==> (
        "direct:slack/T/U",
        "direct:slack/T/U/1712.3",
        EdgeName.Slack,
        Audience.Person,
        false,
        Focus.Focused,
        false
      )
    }

    test("a direct message is only ever with an account a source names, never local or grit") {
      val error =
        assertCompileError("Origin.Direct(grit.core.identity.Account.Local, \"1712.3\")")
      assert(error.msg.contains("Sourced"))
    }

    test("only a TUI session is held with the operator") {
      val dir = Directory.of("/work").getOrElse(throw new java.lang.AssertionError())
      Origin.Tui(dir, "s").audience.operator ==> true
      Origin.Slack("T1", "C1", "1.0").audience.operator ==> false
      Origin.Task("nightly", "1").audience.operator ==> false
    }
  }
}
