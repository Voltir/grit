package grit.claudecode

import java.nio.file.{Files, Path}
import java.time.Instant

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

import grit.commands.routes.RouteFixtures.AgentText
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

  /** Each batch the edge was posted, as whether it is an agent's and its events' kinds. */
  private def kinds(f: HookFixtures): Vector[(Boolean, Vector[String])] =
    f.recorded.map(b => (b.parent.isDefined, b.events.map(_.kind.name)))

  /** The payload of a Bash tool's use by an agent of the session `First`. */
  private def agentsTool(f: HookFixtures): String =
    f.payload("post-tool-use-bash-agent.json").replace(Forked, First)

  /** The key of the queue of that agent's lines. */
  private val Agent = s"$First+a2222222222222222"

  /** That agent, as a line on stderr names it. */
  private val TheAgent = s"the agent a2222222222222222 of the session $First"

  /** An ordinal as the hook writes one, of a moment `ago` before now, `k` its last digits. */
  private def ordinal(ago: FiniteDuration, k: Int = 0): String =
    f"${(System.currentTimeMillis() - ago.toMillis) * 1000000L + k}%019d"

  /** A line the edge takes, of a Bash tool's use in `f`'s checkout by the session `local`, its
    * ordinal `n` and its instant the one `n` reads.
    */
  private def line(f: HookFixtures, local: String, n: String): String = {
    val at = Instant.EPOCH.plusNanos(n.toLong / 1000 * 1000)
    s"""{"session":"$local","dir":"${f.proj}","n":"$n","at":"$at","event":"ran","shape":{"program":"x","ok":true}}"""
  }

  /** The stderr line saying the `k` oldest events kept for the session `First` were dropped. */
  private def dropped(k: Int, why: String): String =
    if (k == 1) said(s"dropped the oldest event kept for the session $First: $why")
    else said(s"dropped the $k oldest events kept for the session $First: $why")

  /** The stderr line saying an event was kept as the edge at `edge` did not answer. */
  private def missed(edge: String, what: String = "PostToolUse"): String =
    said(s"$what was kept to send later: the edge at $edge did not answer")

  /** The hook's sessions directory in `f`. */
  private def sessions(f: HookFixtures): Path = f.hook.resolveSibling("sessions")

  /** `k` of the hook's `event` of `payload` run at once in `f`, given `env`. */
  private def atOnce(f: HookFixtures, k: Int, payload: String, env: Map[String, String]) =
    Await.result(Future.sequence(Vector.fill(k)(Future(f.run("event", payload, env)))), 60.seconds)

  /** The shas of `n` commits made in `f`'s checkout outside any session, oldest first. */
  private def commitsIn(f: HookFixtures, n: Int): Vector[String] = {
    val made = Bash.run(
      f.proj,
      f.machine.home,
      f.machine.bin,
      Seq(
        "-c",
        s"for i in $$(seq $n); do git -c user.name=t -c user.email=t@example.invalid commit -q --allow-empty -m c$$i; done; git rev-list --reverse -n $n HEAD"
      )
    )
    made.out
  }

  val tests = Tests {
    test(
      "an event the edge did not answer is kept, and sent ahead of the session's next in its ordinals' order"
    ) {
      HookFixtures { f =>
        val down = f.down
        val runs =
          Vector("post-tool-use-read.json", "post-tool-use-edit.json", "post-tool-use-bash.json")
            .map(p => f.run("event", f.payload(p), down))
        val queued = f.queue(First)
        val sent = f.run("event", f.payload("post-tool-use-write.json"))
        val posted = arrived(f)
        (
          runs.map(r => (r.exit, r.err)),
          queued.size,
          posted.map(_._2.init),
          posted.forall(_._2.lastOption.exists(n => queued.lastOption.exists(n > _.toLong))),
          sent.err,
          f.queue(First),
          f.logged(First)
        ) ==> (
          runs.map(_ => (0, missed(down("GRIT_EDGE")))),
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
    test("a commit the edge took sends the session's other queues") {
      HookFixtures { f =>
        val _ = f.run("event", agentsTool(f), f.down)
        val made = Bash.run(
          f.proj,
          f.machine.home,
          f.machine.bin,
          Seq(
            "-c",
            "git -c user.name=t -c user.email=t@example.invalid commit -q --allow-empty -m x"
          ),
          env = Map("CLAUDE_CODE_SESSION_ID" -> First)
        )
        (made.err, kinds(f), f.queue(Agent)) ==> (
          said(s"sent 1 event kept earlier for $TheAgent"),
          Vector((false, Vector("commit")), (true, Vector("ran"))),
          Vector.empty
        )
      }
    }
    test(
      "hooks at once the edge does not answer each keep their line, and the next post sends them in their ordinals' order ahead of its own"
    ) {
      HookFixtures { f =>
        // Eight started together: their overlap is likely, not forced.
        val runs = atOnce(f, 8, f.payload("post-tool-use-read.json"), f.down)
        val queued = f.queue(First)
        val _ = f.run("event", f.payload("post-tool-use-edit.json"))
        (runs.map(_.exit), queued.size, arrived(f).map(_._2.init)) ==> (
          Vector.fill(8)(0),
          8,
          Vector(queued.map(_.toLong))
        )
      }
    }
    test("a post the edge took sends the session's other queues, a post each") {
      HookFixtures { f =>
        val _ = f.run("event", agentsTool(f), f.down)
        val run = f.run("event", f.payload("post-tool-use-edit.json"))
        (run.err, kinds(f), f.queue(Agent)) ==> (
          said(s"sent 1 event kept earlier for $TheAgent"),
          Vector((false, Vector("edited")), (true, Vector("ran"))),
          Vector.empty
        )
      }
    }
    test(
      "a queue the edge left unanswered after taking the post before it keeps its lines"
    ) {
      HookFixtures { f =>
        val _ = f.run("event", f.payload("post-tool-use-read.json"), f.down)
        val _ = f.run("event", agentsTool(f), f.down)
        val agents = f.queue(Agent)
        val (sent, asked) = HookFixtures.relaying(f.port, HookFixtures.Relay.First) {
          (proxy, accepted) =>
            val run = f.run(
              "event",
              f.payload("post-tool-use-edit.json"),
              f.claude + ("GRIT_EDGE" -> s"http://127.0.0.1:$proxy")
            )
            (run, accepted())
        }
        (
          sent.err,
          asked,
          arrived(f).map(_._2.size),
          f.queue(First),
          f.queue(Agent),
          agents.size
        ) ==> (
          said(s"sent 1 event kept earlier for the session $First"),
          2,
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
        val lost = HookFixtures.relaying(f.port, HookFixtures.Relay.Lost) { (proxy, _) =>
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
    test(
      "a start's opening sends every other session's queue, and its start's event its own session's, ahead of the start"
    ) {
      HookFixtures { f =>
        val _ = f.run("event", f.payload("post-tool-use-read.json"), f.down)
        val _ = f.run("event", f.payload("post-tool-use-edit.json").replace(First, Forked), f.down)
        val _ = f.run("event", f.payload("post-tool-use-bash-agent.json"), f.down)
        val start = f.payload("session-start-fork.json")
        val _ = f.run("opening", start)
        val opened = f.recorded.map(b => (b.id, b.events.map(_.kind.name)))
        val _ = f.run("event", start)
        (
          opened,
          kinds(f).drop(opened.size),
          f.recorded.drop(opened.size).map(_.id).headOption
        ) ==> (
          Vector(f.session(First) -> Vector("read")),
          Vector((false, Vector("edited", "started")), (true, Vector("ran"))),
          Some(f.session(Forked))
        )
      }
    }
    test("a start's opening stops sending queues at the first the edge leaves unanswered") {
      HookFixtures { f =>
        val _ = f.keep(Forked, ordinal(1.minute), line(f, Forked, ordinal(1.minute)))
        val _ = f.keep("third", ordinal(1.minute), line(f, "third", ordinal(1.minute)))
        val asked = HookFixtures.relaying(f.port, HookFixtures.Relay.First) { (proxy, accepted) =>
          val _ = f.run(
            "opening",
            f.payload("session-start-startup.json"),
            f.claude + ("GRIT_EDGE" -> s"http://127.0.0.1:$proxy")
          )
          accepted()
        }
        // The holder's, then the first queue's.
        (asked, f.queue(Forked).size, f.queue("third").size) ==> (2, 1, 1)
      }
    }
    test(
      "a post the edge answers with refused lines removes the kept lines it carried, and the session's other queues and a start's go on"
    ) {
      HookFixtures { f =>
        val _ = f.keep(First, ordinal(1.minute), "{}")
        val _ = f.run("event", agentsTool(f), f.down)
        val run = f.run("event", f.payload("post-tool-use-edit.json"))
        val event = (run.err, kinds(f), f.queue(First), f.queue(Agent))
        val before = f.recorded.size
        val _ = f.keep(Forked, ordinal(2.minutes), "{}")
        val _ = f.keep(Forked, ordinal(1.minute), line(f, Forked, ordinal(1.minute)))
        val _ = f.keep("third", ordinal(1.minute), line(f, "third", ordinal(1.minute)))
        val _ = f.run("opening", f.payload("session-start-startup.json"))
        (event, f.recorded.drop(before).map(_.id)) ==> (
          (
            said(
              s"sent 1 event kept earlier for the session $First",
              "PostToolUse: line 1 refused: session: missing",
              s"sent 1 event kept earlier for $TheAgent"
            ),
            Vector((false, Vector("edited")), (true, Vector("ran"))),
            Vector.empty,
            Vector.empty
          ),
          Vector(f.session(Forked), f.session("third"))
        )
      }
    }
    test(
      "a post the edge refuses whole drops the kept lines it carried with its own, saying so"
    ) {
      HookFixtures { f =>
        // A line of another session in the queue: the edge takes one session's lines a post.
        val _ = f.keep(First, ordinal(1.minute), line(f, Forked, ordinal(1.minute)))
        val run = f.run("event", f.payload("post-tool-use-edit.json"))
        (
          run.err.startsWith(
            said(
              s"the edge at ${f.edge} refused PostToolUse and the 1 event kept before it whole (400), so they were dropped: "
            ).trim
          ),
          run.err.linesIterator.size,
          f.queue(First),
          f.recorded
        ) ==> (true, 1, Vector.empty, Vector.empty)
      }
    }
    test("a post the edge refuses this machine's credential for is kept, saying what to do") {
      HookFixtures.credentialed(AgentText) { f =>
        val file = HookFixtures.credentialOf(f.machine, "unknown-0123456789abcdefghijklmnop")
        val run = f.run("event", f.payload("post-tool-use-edit.json"))
        (
          run.err.startsWith(
            s"grit claude-code hook: the edge at ${f.edge} refused this machine's credential (401) for PostToolUse: "
          ),
          run.err.endsWith(
            s"; ask whoever runs the edge for this credential and put it in $file; PostToolUse was kept to send later\n"
          ),
          f.queue(First).size
        ) ==> (true, true, 1)
      }
    }
    test("an event that cannot be kept is dropped, saying where it could not be kept") {
      HookFixtures { f =>
        val _ = f.machine.write(sessions(f).resolve(".kept"), "no directory")
        val run = f.run("event", f.payload("post-tool-use-edit.json"), f.down)
        run.err ==> said(
          s"PostToolUse was dropped: it could not be kept in ${sessions(f).resolve(".kept").resolve(First)}"
        )
      }
    }
    test("an event with no time left for its post is kept, saying so") {
      HookFixtures { f =>
        val hurried = f.variant("hurried", "\nbound=3\n" -> "\nbound=0\n")
        val run = f.run("event", f.payload("post-tool-use-edit.json"), program = hurried)
        (run.err, f.queue(First).size, f.recorded) ==> (
          said("PostToolUse was kept to send later: no time was left for its post"),
          1,
          Vector.empty
        )
      }
    }
    test("a rewrite's posts after one the edge did not answer are kept without asking, saying so") {
      HookFixtures { f =>
        val shas = commitsIn(f, 3)
        val chunked = f.variant("chunked", ".size + $k > 262144 then" -> ".size + $k > 1 then")
        val down = f.down
        val run = f.run(
          "rewrite",
          shas.map(s => s"$s $s").mkString("\n"),
          Map("CLAUDE_CODE_SESSION_ID" -> First, "GRIT_EDGE" -> down("GRIT_EDGE")),
          args = Seq("rebase"),
          program = chunked
        )
        (shas.size, run.err, f.queue(First).size) ==> (
          3,
          missed(down("GRIT_EDGE"), "1 of the 3 commits made") +
            said(
              "1 of the 3 commits made was kept to send later, as the post before it was",
              "1 of the 3 commits made was kept to send later, as the post before it was"
            ),
          3
        )
      }
    }
    test("a hook naming no count of unended seconds does nothing, saying so") {
      HookFixtures { f =>
        val broken = f.variant("broken", "\nunended=86400\n" -> "\nunended=x\n")
        val event = f.run("event", f.payload("post-tool-use-edit.json"), program = broken)
        val holder = f.run("holder", "", program = broken)
        val header = f.run("header", "", program = broken)
        (
          (event.exit, event.err),
          (holder.exit, holder.err),
          header.exit,
          f.recorded
        ) ==> (
          (0, said("names no count of seconds after which a kept event is moot; it did nothing")),
          (1, said("names no count of seconds after which a kept event is moot; it did nothing")),
          1,
          Vector.empty
        )
      }
    }
    test("a queue past 1000 lines drops its oldest, saying so") {
      HookFixtures { f =>
        val names = (0 until 1001).map(k => ordinal(1.minute, k)).toVector
        names.foreach(f.keep(First, _, "{}"))
        val down = f.down
        val run =
          f.run("event", f.payload("post-tool-use-read.json"), down, program = f.unbounded)
        val queued = f.queue(First)
        (
          run.err,
          queued.size,
          queued.headOption,
          queued.lastOption.exists(_ > names.last)
        ) ==> (
          dropped(2, "more than 1000 were kept") + missed(down("GRIT_EDGE")),
          1000,
          names.lift(2),
          true
        )
      }
    }
    test("a queue past 256 KiB drops its oldest lines until it fits, saying so") {
      HookFixtures { f =>
        val names = (0 until 4).map(k => ordinal(1.minute, k)).toVector
        names.foreach(f.keep(First, _, "x" * 100000))
        val run = f.run(
          "event",
          f.payload("post-tool-use-read.json"),
          f.down,
          program = f.unbounded
        )
        (run.err.linesIterator.toVector.headOption, f.queue(First).take(2)) ==> (
          Some(dropped(2, "more than 256 KiB were kept").trim),
          names.drop(2)
        )
      }
    }
    test(
      "a queue whose kept lines and new one come to 256 KiB drops nothing, and one byte more drops its oldest"
    ) {
      HookFixtures { f =>
        val payload = f.payload("post-tool-use-read.json")
        def run() = f.run("event", payload, f.down, program = f.unbounded)
        def clear() = f
          .queue(First)
          .foreach(n => Files.delete(sessions(f).resolve(".kept").resolve(First).resolve(n)))
        val _ = run()
        val fresh = f
          .queue(First)
          .map(n => Files.size(sessions(f).resolve(".kept").resolve(First).resolve(n)))
        clear()
        val (a, b) = (ordinal(2.minutes), ordinal(1.minute))
        val rest = 262144 - 3 - fresh.sum.toInt - 1000
        val _ = f.keep(First, a, "x" * 1000)
        val _ = f.keep(First, b, "x" * rest)
        val fits = run()
        val kept = f.queue(First).size
        clear()
        val _ = f.keep(First, a, "x" * 1000)
        val _ = f.keep(First, b, "x" * (rest + 1))
        val over = run()
        (
          fresh.size,
          fits.err.contains("dropped"),
          kept,
          over.err.linesIterator.size,
          f.queue(First).headOption
        ) ==> (
          1,
          false,
          3,
          2,
          Some(b)
        )
      }
    }
    test(
      "a kept line older than a session's unended span is dropped before its queue is posted, and a younger one sent"
    ) {
      HookFixtures { f =>
        val (old, young) = (ordinal(25.hours), ordinal(23.hours))
        val _ = f.keep(First, old, line(f, First, old))
        val _ = f.keep(First, young, line(f, First, young))
        val run = f.run("event", f.payload("post-tool-use-read.json"))
        (run.err, arrived(f).map(_._2.init)) ==> (
          dropped(1, "older than 24 hours") +
            said(s"sent 1 event kept earlier for the session $First"),
          Vector(Vector(young.toLong))
        )
      }
    }
    test("a queue drops lines past the unended span before those past its count") {
      HookFixtures { f =>
        val _ = f.keep(First, ordinal(25.hours), "{}")
        (0 until 1000).foreach(k => f.keep(First, ordinal(1.minute, k), "{}"))
        val down = f.down
        val run =
          f.run("event", f.payload("post-tool-use-read.json"), down, program = f.unbounded)
        (run.err, f.queue(First).size) ==> (
          dropped(1, "older than 24 hours") + dropped(1, "more than 1000 were kept") +
            missed(down("GRIT_EDGE")),
          1000
        )
      }
    }
    test("a new line alone past the byte bound drops every kept line and is posted") {
      HookFixtures { f =>
        val small = f.variant("small", "\nmost_bytes=262144\n" -> "\nmost_bytes=2048\n")
        val _ = f.keep(First, ordinal(2.minutes), "{}")
        val _ = f.keep(First, ordinal(1.minute), "{}")
        val stop = ujson.read(f.payload("stop.json"))
        stop("last_assistant_message") = "y" * 1990
        val run = f.run("event", ujson.write(stop), program = small)
        (run.err, kinds(f), f.queue(First)) ==> (
          dropped(2, "more than 2 KiB were kept"),
          Vector((false, Vector("closing"))),
          Vector.empty
        )
      }
    }
    test("a queue's entry no ordinal names is never posted") {
      HookFixtures { f =>
        val n = ordinal(1.minute)
        val _ = f.keep(First, "notes", "{}")
        val _ = f.keep(First, n, line(f, First, n))
        val run = f.run("event", f.payload("post-tool-use-read.json"))
        (run.err, arrived(f).map(_._2.init), f.queue(First)) ==> (
          said(s"sent 1 event kept earlier for the session $First"),
          Vector(Vector(n.toLong)),
          Vector("notes")
        )
      }
    }
    test(
      "a start removes every kept line and temporary file older than the unended span, any entry no ordinal names, and the queues left empty"
    ) {
      HookFixtures { f =>
        val _ = f.keep(Forked, ordinal(25.hours), "{}")
        val _ = f.keep(Forked, s".${ordinal(25.hours)}.abcdef", "{")
        val young = ordinal(1.hour)
        val _ = f.keep(s"$Forked+a1", ordinal(30.hours), "{}")
        val _ = f.keep(s"$Forked+a1", "notes", "{}")
        val _ = f.keep(s"$Forked+a1", s".$young.abcdef", "{")
        val kept = sessions(f).resolve(".kept")
        val _ = f.machine.write(kept.resolve("stray"), "x")
        val run = f.run("opening", f.payload("session-start-startup.json"))
        (
          run.err,
          Files.exists(kept.resolve(Forked)),
          Files.exists(kept.resolve("stray")),
          f.queue(s"$Forked+a1")
        ) ==> (
          said(s"dropped the oldest event kept for the session $Forked: older than 24 hours") +
            said(
              s"dropped the oldest event kept for the agent a1 of the session $Forked: older than 24 hours"
            ),
          false,
          false,
          Vector(s".$young.abcdef")
        )
      }
    }
  }
}
