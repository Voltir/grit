package grit.core.session

import java.time.Instant

import grit.core.place.{Namespace, Place}

import utest.*

/** What a tool call that ran adds to its session's log, before it is recorded. */
object UnrecordedTests extends TestSuite {

  private val t0 = Instant.parse("2026-10-28T10:00:00Z")
  private val path = Event.Unrecorded.read(Event.Shape.Path, "src/a.scala").shape

  private def event(u: Event.Unrecorded, n: Long, shown: String) =
    Event.of(n, t0, u.kind(shown), u.shape)

  val tests = Tests {
    test("a read counts the UTF-8 bytes of what the model was shown, keeping its shape") {
      val read = event(Event.Unrecorded.Read(path), 4, "héllo")
      assert(
        read.map(e => (e.n, e.at, e.kind, e.shape)) == Right((4L, t0, Event.Kind.Read(6), path))
      )
    }
    test("a commit's subject is cut at a word edge to fit, ending …") {
      val at = Place.under(Namespace.Fs, Vector("/w"))
      val cut = Event.Shape
        .committed(at, "1a2b3c4d5e6f", Some("fixes " * 60))
        .map(s => ujson.read(Event.Shape.value(s)).obj.get(Event.Shape.Subject).flatMap(_.strOpt))
      cut ==> Right(Some(("fixes " * 40).trim + "…"))
    }
    test("a commit's shape keeps its sha and cuts its subject to fit, ending …") {
      val sha = "1a2b3c4d5e6f"
      val at = Place.under(Namespace.Fs, Vector("/w"))
      val short = Event.Shape.committed(at, sha, Some("turn: a thing")).map(Event.Shape.value)
      val long = Event.Shape.committed(at, sha, Some("é" * 400)).map(Event.Shape.value)
      val cut = long.map(ujson.read(_).obj.get(Event.Shape.Subject).flatMap(_.strOpt))
      (
        short,
        long.map(_.getBytes("UTF-8").length <= Event.Shape.Longest),
        cut.map(_.exists(_.endsWith("é…"))),
        cut.map(_.exists(_.length > 100)),
        Event.Unrecorded.Commit(Event.Shape.Empty).kind("x")
      ) ==> (
        Right("""{"worktree":"fs:/w","sha":"1a2b3c4d5e6f","subject":"turn: a thing"}"""),
        Right(true),
        Right(true),
        Right(true),
        Event.Kind.Commit
      )
    }
    test("an edit and a run count no bytes, whatever was shown") {
      assert(
        Event.Unrecorded.edited("src/a.scala") == Event.Unrecorded.Edited(path),
        Event.Unrecorded.Edited(path).kind("done") == Event.Kind.Edited,
        Event.Unrecorded.Ran(Event.Shape.Empty).kind("exit 0") == Event.Kind.Ran
      )
    }
    test("a shape over the longest a shape takes is the empty shape") {
      assert(
        Event.Shape.value(path) == """{"path":"src/a.scala"}""",
        Event.Unrecorded.read(Event.Shape.Path, "a" * Event.Shape.Longest) ==
          Event.Unrecorded.Read(Event.Shape.Empty)
      )
    }
  }
}
