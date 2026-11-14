package grit.commands.events

import java.time.Instant

import grit.commands.worker.{Holders, WorkerName, Worktrees}
import grit.core.edge.EdgeSessions
import grit.core.id.{Harness, SessionId}
import grit.core.place.Directory
import grit.core.session.{Event, Session}

import utest.*

/** [[Lines]]: a harness's event lines read in grit's words, and a post's lines as one batch. */
object LinesTests extends TestSuite {

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)

  private val MadeUp: Harness = got(Harness.of("made-up"))
  private val Desktop: WorkerName = got(WorkerName.of("desktop"))
  private val Now: Instant = Instant.parse("2026-11-13T12:00:00Z")
  private val At: Instant = Now.minusSeconds(30)

  private def dir(path: String): Directory = got(Directory.of(path))
  private def id(local: String): SessionId = got(SessionId.of(MadeUp, local))

  private def line(fields: (String, ujson.Value)*): String =
    ujson.write(
      ujson.Obj.from(
        Seq[(String, ujson.Value)](
          "session" -> "s-1",
          "dir" -> "/work/grit",
          "n" -> "1",
          "at" -> At.toString,
          "event" -> "edited"
        ).filterNot((k, _) => fields.exists(_._1 == k)) ++ fields.filterNot(_._2 == ujson.Null)
      )
    )

  private def read(text: String) = Lines.read(text, MadeUp, Desktop, Now)

  private def shape(o: ujson.Obj): Event.Shape = got(Event.Shape.of(o))

  val tests = Tests {
    test("a line is its session's event, at its worktree on the worker, as its holder") {
      read(line("worktree" -> "/work/fixer", "shape" -> ujson.Obj("path" -> "src/A.scala"))) ==>
        Right(
          Lines.Line(
            id("s-1"),
            None,
            Holders.of(Desktop, dir("/work/grit"), None),
            Worktrees.at(Desktop, dir("/work/fixer")),
            got(Event.of(1, At, Event.Kind.Edited, shape(ujson.Obj("path" -> "src/A.scala"))))
          )
        )
    }

    test(
      "an agent's line is its own session, a child of the one starting it, as the agent's holder"
    ) {
      read(line("agent" -> "a-1", "type" -> "coder", "event" -> "started")) ==>
        Right(
          Lines.Line(
            id("s-1/a-1"),
            Some(id("s-1")),
            Holders.of(Desktop, dir("/work/grit"), Some(Holders.Agent("coder", id("s-1")))),
            Worktrees.at(Desktop, dir("/work/grit")),
            got(Event.of(1, At, Event.Kind.Started, Event.Shape.Empty))
          )
        )
    }

    test(
      "a commit keeps every key its shape gave, at its worktree as a place, its subject cut to fit"
    ) {
      val besides = ujson.Obj("child" -> true, "replaces" -> ujson.Arr("0123456789ab"))
      val worktree = Worktrees.at(Desktop, dir("/work/grit"))
      read(
        line(
          "event" -> "commit",
          "shape" -> ujson.Obj(
            "sha" -> "1a2b3c4d5e6f",
            "subject" -> ("fixes " * 60),
            "child" -> true,
            "replaces" -> ujson.Arr("0123456789ab"),
            "worktree" -> "/elsewhere"
          )
        )
      ).map(_.event.shape) ==>
        Event.Shape.committed(worktree, "1a2b3c4d5e6f", Some("fixes " * 60), besides)
    }

    test(
      "an edit's own worktree is written as a place on the worker; a read counts its bytes; a closing's text is its summary"
    ) {
      (
        read(line("shape" -> ujson.Obj("path" -> "A.scala", "worktree" -> "/work/b")))
          .map(_.event.shape),
        read(line("event" -> "read", "bytes" -> 2048)).map(_.event.kind),
        read(line("event" -> "closing", "text" -> "Did the thing.")).map(_.event.kind)
      ) ==> (
        Right(
          shape(
            ujson
              .Obj("path" -> "A.scala", "worktree" -> Worktrees.at(Desktop, dir("/work/b")).written)
          )
        ),
        Right(Event.Kind.Read(2048)),
        Right(Event.Kind.Closing(got(Session.Summary.of("Did the thing."))))
      )
    }

    test("each bound a line is refused for is named") {
      Vector(
        "x" * (Lines.Longest + 1),
        "[1]",
        line("slot" -> "x"),
        line("n" -> "0"),
        line("n" -> 1),
        line("at" -> Now.plusMillis(Lines.Skew.toMillis + 1).toString),
        line("event" -> "PostToolUse"),
        line("event" -> "read"),
        line("event" -> "closing"),
        line("text" -> "said"),
        line("agent" -> "a-1"),
        line("dir" -> "work/grit"),
        line("shape" -> ujson.Obj("path" -> "p" * 300)),
        line("event" -> "commit", "shape" -> ujson.Obj("subject" -> "no sha"))
      ).map(read(_).left.toOption) ==> Vector(
        Some(s"a line over ${Lines.Longest} bytes"),
        Some("no JSON object"),
        Some(
          "no key slot is read in a line; these are: session, agent, type, dir, worktree, n, at, event, bytes, shape, text"
        ),
        Some("n: an event's ordinal is above 0: 0"),
        Some("n: a decimal string, not 1"),
        Some(
          s"at: later than ${Lines.Skew.toSeconds}s past this edge's clock: ${Now.plusMillis(Lines.Skew.toMillis + 1)}"
        ),
        Some("event: started, read, edited, ran, commit, closing or ended, not PostToolUse"),
        Some("bytes: a read's count of bytes, missing"),
        Some("text: a closing's summary, missing"),
        Some("text: a closing's alone"),
        Some("agent and type come together: an agent's own id, and its type"),
        Some("dir: not an absolute path: work/grit"),
        Some("shape: an event's shape is 311 bytes, over 300"),
        Some("shape: a commit's sha, missing")
      )
    }

    test("a session or an agent holding '/' is refused, so no two lines' ids make one session") {
      Vector(
        line("session" -> "a/b"),
        line("session" -> "a", "agent" -> "b/c", "type" -> "coder")
      ).map(read(_).left.toOption) ==> Vector(
        Some(
          "session: holds '/', which parts an agent's session from the session starting it: a/b"
        ),
        Some("agent: holds '/', which parts an agent's session from the session starting it: b/c")
      )
    }

    test(
      "a post is one batch of one session's lines, place and holder from its first, each line refused named by number"
    ) {
      val body = Vector(
        line("n" -> "1", "worktree" -> "/work/a"),
        "",
        line("n" -> "0"),
        line("n" -> "2", "worktree" -> "/work/b")
      ).mkString("\n")
      Lines.batch(body, MadeUp, Desktop, Now) ==> Right(
        Lines.Batched(
          Some(
            EdgeSessions.Batch(
              id("s-1"),
              None,
              Worktrees.at(Desktop, dir("/work/a")),
              Some(Holders.of(Desktop, dir("/work/grit"), None)),
              Vector(
                got(Event.of(1, At, Event.Kind.Edited, Event.Shape.Empty)),
                got(Event.of(2, At, Event.Kind.Edited, Event.Shape.Empty))
              )
            )
          ),
          Vector(3 -> "n: an event's ordinal is above 0: 0")
        )
      )
    }

    test("a post of two sessions' lines, of no line, or for grit's own harness, is refused whole") {
      (
        Lines.batch(
          Vector(line(), line("agent" -> "a-1", "type" -> "t")).mkString("\n"),
          MadeUp,
          Desktop,
          Now
        ),
        Lines.batch("\n\n", MadeUp, Desktop, Now),
        Lines.batch(line(), Harness.Grit, Desktop, Now),
        Lines.batch("{", MadeUp, Desktop, Now)
      ) ==> (
        Left("line 2 is session made-up:s-1/a-1, not made-up:s-1: post each session's lines apart"),
        Left("the body holds no line"),
        Left("the harness grit is grit's own: only its turns record its sessions"),
        Right(Lines.Batched(None, Vector(1 -> "no JSON object")))
      )
    }
  }
}
