package grit.core.session

import java.time.Instant

import grit.core.session.Log.Line

import utest.*

/** A session's log, its counters, its ending and an event's shape, as pure functions. */
object SessionLogTests extends TestSuite {

  private val t0 = Instant.parse("2026-10-28T10:00:00Z")

  private def event(n: Long, kind: Event.Kind = Event.Kind.Edited): Event =
    Event.of(n, t0.plusSeconds(n), kind, Event.Shape.Empty).fold(sys.error, identity)

  val tests = Tests {
    test("events are logged at consecutive seqs from the log's next") {
      val lines = Log.admit(7, Vector(event(10), event(11, Event.Kind.Ran)))
      assert(lines == Vector(Line.Logged(7, event(10)), Line.Logged(8, event(11, Event.Kind.Ran))))
    }
    test("the events past the hard cap are one overflow at the cap, for the first of them") {
      val offered = (1L to 4L).map(event(_)).toVector
      val lines = Log.admit(Log.HardCap - 1, offered)
      assert(
        lines == Vector(
          Line.Logged(Log.HardCap - 1, event(1)),
          Line.Overflow(Log.HardCap, 2, t0.plusSeconds(2))
        )
      )
      assert(lines.map(_.kind) == Vector("edited", "overflow"))
    }
    test("a full log takes no line") {
      assert(Log.admit(Log.HardCap + 1, Vector(event(1))) == Vector.empty)
    }
    test("tool calls count reads, edits, runs and commits; bytes count reads") {
      val events = Vector(
        event(1, Event.Kind.Started),
        event(2, Event.Kind.Read(120)),
        event(3, Event.Kind.Read(30)),
        event(4, Event.Kind.Edited),
        event(5, Event.Kind.Ran),
        event(6, Event.Kind.Commit),
        event(7, Event.Kind.Ended)
      )
      assert(
        Session.Counters.of(events) ==
          Session.Counters.read(5, 1, 1, 150).fold(sys.error, identity)
      )
    }
    test("an ended session is opened again by a started event after its end, not before") {
      val ended = event(5, Event.Kind.Ended)
      assert(Session.ending(None, Vector(ended)) == Some(ended.at))
      assert(Session.ending(Some(ended.at), Vector(event(9, Event.Kind.Started))) == None)
      assert(Session.ending(Some(ended.at), Vector(event(2, Event.Kind.Started))) == Some(ended.at))
    }
    test("a shape is held to 300 UTF-8 bytes, not characters") {
      // {"p":"…"} is 8 bytes around its text; each é is 2 bytes.
      val at = Event.Shape.of(ujson.Obj("p" -> ("é" * 146)))
      val over = Event.Shape.of(ujson.Obj("p" -> ("é" * 146 + "x")))
      assert(at.map(s => Event.Shape.value(s).length) == Right(154))
      assert(over == Left("an event's shape is 301 bytes, over 300"))
    }
    test("an event's ordinal is above 0 and a read's bytes are not negative") {
      assert(
        Event.of(0, t0, Event.Kind.Ran, Event.Shape.Empty) == Left(
          "an event's ordinal is above 0: 0"
        )
      )
      assert(
        Event.of(1, t0, Event.Kind.Read(-1), Event.Shape.Empty) ==
          Left("a read is of 0 bytes or more: -1")
      )
    }
  }
}
