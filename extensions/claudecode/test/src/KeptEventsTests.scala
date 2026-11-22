package grit.claudecode

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

import grit.core.edge.EdgeSessions

import utest.*

/** The events the edge did not take, kept beside the hook a queue a session (and one an agent of
  * it), against the commands edge: a line never reaches the edge before a line already in its
  * queue when its hook read the queue, kept lines go in their ordinals' order, and the edge logs
  * each line once however often it was posted.
  */
object KeptEventsTests extends TestSuite {

  import HookFixtures.{First, Forked}

  /** What the hook says on stderr, a line `said`. */
  private def said(lines: String*): String = lines.map(l => s"grit claude-code hook: $l\n").mkString

  /** Each batch the edge was posted, as its session and its events' ordinals. */
  private def arrived(f: HookFixtures): Vector[(String, Vector[Long])] =
    f.recorded.map((b: EdgeSessions.Batch) => (b.id.toString, b.events.map(_.n)))

  /** The hook's `event` of `payload` run twice at once in `f`, given `env`. */
  private def twice(f: HookFixtures, payload: String, env: Map[String, String]): Vector[Bash.Done] =
    Await.result(
      Future.sequence(Vector.fill(2)(Future(f.run("event", payload, env)))),
      30.seconds
    )

  /** The payload of a Bash tool's use by an agent of the session `First`. */
  private def agentsTool(f: HookFixtures): String =
    f.payload("post-tool-use-bash-agent.json").replace(Forked, First)

  /** The key of the queue of that agent's lines. */
  private val Agent = s"$First+a2222222222222222"

  /** An ordinal as the hook writes one, of a moment `ago` before now, `k` its last digits. */
  private def ordinal(ago: FiniteDuration, k: Int = 0): String =
    f"${(System.currentTimeMillis() - ago.toMillis) * 1000000L + k}%019d"

  /** The stderr line saying the `k` oldest events kept for the session `First` were dropped. */
  private def dropped(k: Int, why: String): String =
    if (k == 1) said(s"dropped the oldest event kept for the session $First: $why")
    else said(s"dropped the $k oldest events kept for the session $First: $why")

  val tests = Tests {
    test("a queue past 1000 lines drops its oldest, saying so") {
      HookFixtures { f =>
        val names = (0 until 1000).map(k => ordinal(1.minute, k)).toVector
        names.foreach(f.keep(First, _, "{}"))
        val down = f.down
        val run = f.run("event", f.payload("post-tool-use-read.json"), down)
        val queued = f.queue(First)
        (
          run.err,
          queued.size,
          queued.headOption,
          queued.lastOption.exists(_ > names.last)
        ) ==> (
          dropped(1, "more than 1000 were kept") +
            said(
              s"PostToolUse was kept to send later: the edge at ${down("GRIT_EDGE")} did not answer"
            ),
          1000,
          names.lift(1),
          true
        )
      }
    }
    test("a queue past 256 KiB drops its oldest lines until it fits, saying so") {
      HookFixtures { f =>
        val names = (0 until 3).map(k => ordinal(1.minute, k)).toVector
        names.foreach(f.keep(First, _, "x" * 100000))
        val run = f.run("event", f.payload("post-tool-use-read.json"), f.down)
        (run.err.linesIterator.toVector.headOption, f.queue(First).take(2)) ==> (
          Some(dropped(1, "more than 256 KiB were kept").trim),
          names.drop(1)
        )
      }
    }
    test("a kept line older than a session's unended span is dropped before its queue is posted") {
      HookFixtures { f =>
        val old = ordinal(25.hours)
        val _ = f.keep(First, old, "{}")
        val run = f.run("event", f.payload("post-tool-use-read.json"))
        (run.err, f.recorded.map(_.events.size)) ==> (dropped(1, "older than 24 hours"), Vector(1))
      }
    }
    test(
      "a start removes every kept line and temporary file older than the unended span, and the queues left empty"
    ) {
      HookFixtures { f =>
        val _ = f.keep(Forked, ordinal(25.hours), "{}")
        val _ = f.keep(Forked, s".${ordinal(25.hours)}.abcdef", "{")
        val young = ordinal(1.hour)
        val _ = f.keep(s"$Forked+a1", ordinal(30.hours), "{}")
        val _ = f.keep(s"$Forked+a1", s".$young.abcdef", "{")
        val run = f.run("opening", f.payload("session-start-startup.json"))
        val kept = f.hook.resolveSibling("sessions").resolve(".kept")
        (
          run.err,
          java.nio.file.Files.exists(kept.resolve(Forked)),
          f.queue(s"$Forked+a1")
        ) ==> (
          said(s"dropped the oldest event kept for the session $Forked: older than 24 hours") +
            said(
              s"dropped the oldest event kept for the agent a1 of the session $Forked: older than 24 hours"
            ),
          false,
          Vector(s".$young.abcdef")
        )
      }
    }
    test("a start sends its own session's queues first, then every other queue, a post each") {
      HookFixtures { f =>
        val _ = f.run("event", f.payload("post-tool-use-read.json").replace(First, Forked), f.down)
        val _ = f.run("event", agentsTool(f), f.down)
        val _ = f.run("event", f.payload("post-tool-use-edit.json"), f.down)
        val _ = f.run("opening", f.payload("session-start-startup.json"))
        (
          f.recorded.map(b => (b.parent.isDefined, b.events.map(_.kind.name))),
          f.recorded.lastOption.map(_.id)
        ) ==> (
          Vector((false, Vector("edited")), (true, Vector("ran")), (false, Vector("read"))),
          Some(f.session(Forked))
        )
      }
    }
    test(
      "an event the edge did not answer is kept, and sent ahead of the session's next in its ordinals' order, each logged once"
    ) {
      HookFixtures { f =>
        val down = f.down
        val edge = down("GRIT_EDGE")
        val missed =
          Vector("post-tool-use-read.json", "post-tool-use-edit.json", "post-tool-use-bash.json")
            .map(p => f.run("event", f.payload(p), down))
        val queued = f.queue(First)
        val sent = f.run("event", f.payload("post-tool-use-write.json"))
        val posted = arrived(f)
        (
          missed.map(r => (r.exit, r.err)),
          queued.size,
          posted.map(_._2.init),
          posted.forall(_._2.lastOption.exists(n => queued.lastOption.exists(n > _.toLong))),
          sent.err,
          f.queue(First),
          f.logged(First)
        ) ==> (
          missed.map(_ =>
            (0, said(s"PostToolUse was kept to send later: the edge at $edge did not answer"))
          ),
          3,
          Vector(queued.map(_.toLong)),
          true,
          said(s"sent 3 events kept earlier for the session $First"),
          Vector.empty,
          Some(4L)
        )
      }
    }
    test("a commit the edge did not answer is kept, and sent ahead of the session's next event") {
      HookFixtures { f =>
        val made = Bash.run(
          f.proj,
          f.machine.home,
          f.machine.bin,
          Seq(
            "-c",
            "git -c user.name=t -c user.email=t@example.invalid commit -q --allow-empty -m x"
          ),
          env = Map("CLAUDE_CODE_SESSION_ID" -> First, "GRIT_EDGE" -> f.down("GRIT_EDGE"))
        )
        val queued = f.queue(First)
        val _ = f.run("event", f.payload("post-tool-use-read.json"))
        (
          made.exit,
          queued.size,
          f.recorded.map(_.events.map(_.kind.name)),
          f.queue(First)
        ) ==> (0, 1, Vector(Vector("commit", "read")), Vector.empty)
      }
    }
    test(
      "two hooks at once the edge does not answer each keep their line, and the next post sends both in their ordinals' order ahead of its own"
    ) {
      HookFixtures { f =>
        val missed = twice(f, f.payload("post-tool-use-read.json"), f.down)
        val queued = f.queue(First)
        val _ = f.run("event", f.payload("post-tool-use-edit.json"))
        (missed.map(_.exit), queued.size, arrived(f).map(_._2.init)) ==> (
          Vector(0, 0),
          2,
          Vector(queued.map(_.toLong))
        )
      }
    }
    test("two hooks at once that each post a session's queue have the edge log every line once") {
      HookFixtures { f =>
        val _ = twice(f, f.payload("post-tool-use-read.json"), f.down)
        val sent = twice(f, f.payload("post-tool-use-edit.json"), f.claude)
        (sent.map(_.exit), f.logged(First), f.queue(First)) ==> (
          Vector(0, 0),
          Some(4L),
          Vector.empty
        )
      }
    }
    test(
      "a post the edge took sends the session's other queues; one the edge then left unanswered keeps its lines"
    ) {
      HookFixtures { f =>
        val _ = f.run("event", f.payload("post-tool-use-read.json"), f.down)
        val _ = f.run("event", agentsTool(f), f.down)
        val agents = f.queue(Agent)
        val sent = HookFixtures.relaying(f.port, HookFixtures.Relay.First) { proxy =>
          f.run(
            "event",
            f.payload("post-tool-use-edit.json"),
            f.claude + ("GRIT_EDGE" -> s"http://127.0.0.1:$proxy")
          )
        }
        (
          sent.err,
          arrived(f).map(_._2.size),
          f.queue(First),
          f.queue(Agent),
          agents.size
        ) ==> (
          said(s"sent 1 event kept earlier for the session $First"),
          Vector(2),
          Vector.empty,
          agents,
          1
        )
      }
    }
    test(
      "a post the edge took whose answer was lost is kept, and sent again with the next, the edge logging each line once"
    ) {
      HookFixtures { f =>
        val lost = HookFixtures.relaying(f.port, HookFixtures.Relay.Lost) { proxy =>
          f.run(
            "event",
            f.payload("post-tool-use-read.json"),
            f.claude + ("GRIT_EDGE" -> s"http://127.0.0.1:$proxy")
          )
        }
        val queued = f.queue(First)
        val _ = f.run("event", f.payload("post-tool-use-edit.json"))
        (
          lost.exit,
          queued.size,
          arrived(f).map(_._2.size),
          f.logged(First),
          f.queue(First)
        ) ==> (0, 1, Vector(1, 2), Some(2L), Vector.empty)
      }
    }
  }
}
