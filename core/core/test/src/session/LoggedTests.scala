package grit.core.session

import java.time.Instant

import utest.*

/** What a tool call that ran adds to its session's log, as the event it becomes. */
object LoggedTests extends TestSuite {

  private val t0 = Instant.parse("2026-10-28T10:00:00Z")
  private val path = Logged.shaped(Event.Shape.Path, "src/a.scala")

  val tests = Tests {
    test("a read counts the UTF-8 bytes of what the model was shown, keeping its shape") {
      val read = Logged.Read(path).event(4, t0, "héllo")
      assert(
        read.map(e => (e.n, e.at, e.kind, e.shape)) == Right((4L, t0, Event.Kind.Read(6), path))
      )
    }
    test("a commit's shape keeps its sha and cuts its subject to fit, ending …") {
      val sha = "1a2b3c4d5e6f"
      val short = Event.Shape.commit(sha, Some("turn: a thing")).map(Event.Shape.value)
      val long = Event.Shape.commit(sha, Some("é" * 400)).map(Event.Shape.value)
      val cut = long.map(ujson.read(_).obj.get(Event.Shape.Subject).flatMap(_.strOpt))
      (
        short,
        long.map(_.getBytes("UTF-8").length <= Event.Shape.Longest),
        cut.map(_.exists(_.endsWith("é…"))),
        cut.map(_.exists(_.length > 100)),
        Logged.Commit(Event.Shape.Empty).event(7, t0, "x").map(_.kind)
      ) ==> (
        Right("""{"sha":"1a2b3c4d5e6f","subject":"turn: a thing"}"""),
        Right(true),
        Right(true),
        Right(true),
        Right(Event.Kind.Commit)
      )
    }
    test("an edit and a run count no bytes, whatever was shown") {
      assert(
        Logged.Edited(path).event(5, t0, "done").map(_.kind) == Right(Event.Kind.Edited),
        Logged.Ran(Event.Shape.Empty).event(6, t0, "exit 0").map(_.kind) == Right(Event.Kind.Ran)
      )
    }
    test("a shape over the longest a shape takes is the empty shape") {
      assert(
        Event.Shape.value(path) == """{"path":"src/a.scala"}""",
        Logged.shaped(Event.Shape.Path, "a" * Event.Shape.Longest) == Event.Shape.Empty
      )
    }
  }
}
