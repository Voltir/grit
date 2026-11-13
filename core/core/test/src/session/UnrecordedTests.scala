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
    test("a commit's shape keeps every other key it is given while its subject is cut") {
      val at = Place.under(Namespace.Fs, Vector("/w"))
      val besides = ujson.Obj(
        Event.Shape.Child -> true,
        Event.Shape.Replaces -> ujson.Arr("0123456789ab"),
        Event.Shape.Sha -> "not-this-one"
      )
      val shape = Event.Shape
        .committed(at, "1a2b3c4d5e6f", Some("fixes " * 60), besides)
        .map(s => ujson.read(Event.Shape.value(s)).obj)
      val subject = shape.map(_.get(Event.Shape.Subject).flatMap(_.strOpt).map(_.takeRight(6)))
      (
        shape.map(o => o.keys.toVector),
        shape.map(_.get(Event.Shape.Sha).map(_.str)),
        shape.map(_.get(Event.Shape.Replaces)),
        subject
      ) ==> (
        Right(
          Vector(
            Event.Shape.Worktree,
            Event.Shape.Sha,
            Event.Shape.Child,
            Event.Shape.Replaces,
            Event.Shape.Subject
          )
        ),
        Right(Some("1a2b3c4d5e6f")),
        Right(Some(ujson.Arr("0123456789ab"))),
        Right(Some("fixes…"))
      )
    }

    test("a commit's shape keeps its sha and cuts its subject to fit, ending …") {
      val sha = "1a2b3c4d5e6f"
      val at = Place.under(Namespace.Fs, Vector("/w"))
      val short = Event.Shape.committed(at, sha, Some("turn: a thing")).map(Event.Shape.value)
      val long = Event.Shape.committed(at, sha, Some("é" * 400)).map(Event.Shape.value)
      // 52 bytes before the subject and 2 after leave it 246: 121 two-byte é and a three-byte …
      (
        short,
        long,
        Event.Unrecorded.Commit(Event.Shape.Empty).kind("x")
      ) ==> (
        Right("""{"worktree":"fs:/w","sha":"1a2b3c4d5e6f","subject":"turn: a thing"}"""),
        Right(s"""{"worktree":"fs:/w","sha":"1a2b3c4d5e6f","subject":"${"é" * 121}…"}"""),
        Event.Kind.Commit
      )
    }
    test("an edit is of kind edited, at its path, and a run of kind ran, whatever was shown") {
      assert(
        Event.Unrecorded.edited("src/a.scala") == Event.Unrecorded.Edited(path),
        Event.Unrecorded.Edited(path).kind("done") == Event.Kind.Edited,
        Event.Unrecorded.Ran(Event.Shape.Empty).kind("exit 0") == Event.Kind.Ran
      )
    }
    test("a shape at the longest a shape takes is kept, and one a byte over is the empty shape") {
      // {"path":""} is 11 bytes, so a path of 289 makes a shape of exactly Longest, 300.
      val at = "a" * (Event.Shape.Longest - 11)
      (
        Event.Shape.value(path),
        Event.Unrecorded.read(Event.Shape.Path, at) match {
          case Event.Unrecorded.Read(shape) => Event.Shape.value(shape)
          case other => other.toString
        },
        Event.Unrecorded.read(Event.Shape.Path, at + "a")
      ) ==> (
        """{"path":"src/a.scala"}""",
        s"""{"path":"$at"}""",
        Event.Unrecorded.Read(Event.Shape.Empty)
      )
    }
  }
}
