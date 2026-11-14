package grit.claudecode

import scala.concurrent.duration.*

import grit.core.host.Argv

import utest.*

/** The hook when the edge is down, silent, failing or misnamed, or its input never ends: it exits 0
  * (`holder`, 1) within its bound, and says on stderr what it could not do.
  */
object DownTests extends TestSuite {

  private def context(event: String, text: String): ujson.Value =
    ujson.Obj(
      "hookSpecificOutput" -> ujson.Obj("hookEventName" -> event, "additionalContext" -> text)
    )

  private def printed(done: Bash.Done): ujson.Value =
    if (done.out.isEmpty) ujson.Null else ujson.read(done.out.mkString("\n"))

  /** The line a start's brief gives when the edge at `edge` did not answer. */
  private def unanswered(edge: String): ujson.Value =
    context(
      "SessionStart",
      s"grit: grit's server at $edge did not answer. This session starts without its brief, " +
        "and what it does is not recorded until the server answers."
    )

  val tests = Tests {
    test(
      "with the edge's port closed, brief alone gives one line of context, each start role and holder ends at once, each saying so in one line"
    ) {
      HookFixtures { f =>
        val edge = s"http://127.0.0.1:${HookFixtures.closedPort}"
        val env = f.claude + ("GRIT_EDGE" -> edge)
        val start = f.payload("session-start-startup.json")
        val (brief, briefMs) = f.timed("brief", start, env)
        val (opening, openingMs) = f.timed("opening", start, env)
        val (holder, holderMs) = f.timed("holder", "", env)
        (
          (brief.exit, printed(brief), brief.err.linesIterator.size),
          (opening.exit, opening.out, opening.err.linesIterator.size),
          (holder.exit, holder.out, holder.err.linesIterator.size)
        ) ==> ((0, unanswered(edge), 1), (0, Vector.empty, 1), (1, Vector.empty, 1))
        // At once: well short of the 3 s bound a request that waited would reach.
        assert(briefMs < 2500, openingMs < 2500, holderMs < 2500)
      }
    }
    test(
      "with an edge that accepts and never answers, brief ends within its bound, saying the edge did not answer"
    ) {
      HookFixtures { f =>
        HookFixtures.silent { port =>
          val edge = s"http://127.0.0.1:$port"
          val (brief, ms) = f.timed(
            "brief",
            f.payload("session-start-startup.json"),
            f.claude + ("GRIT_EDGE" -> edge)
          )
          (brief.exit, printed(brief)) ==> (0, unanswered(edge))
          // The 3 s bound, short of the 5 s Claude Code gives the handler.
          assert(ms < 4500)
        }
      }
    }
    test(
      "with an edge that accepts and never answers, opening ends within its bound and prints nothing"
    ) {
      HookFixtures { f =>
        HookFixtures.silent { port =>
          val env = f.claude + ("GRIT_EDGE" -> s"http://127.0.0.1:$port")
          val (opening, ms) = f.timed("opening", f.payload("session-start-compact.json"), env)
          (opening.exit, opening.out) ==> (0, Vector.empty)
          // The 3 s bound, short of the 5 s Claude Code gives the handler.
          assert(ms < 4500)
        }
      }
    }
    test(
      "with an edge that accepts and never answers, holder exits 1 within its bound, saying so"
    ) {
      HookFixtures { f =>
        HookFixtures.silent { port =>
          val env = Map("CLAUDE_CODE_SESSION_ID" -> "s-1", "GRIT_EDGE" -> s"http://127.0.0.1:$port")
          val (holder, ms) = f.timed("holder", "", env)
          (holder.exit, holder.out, holder.err.linesIterator.size) ==> (1, Vector.empty, 1)
          // The 3 s bound, short of the 5 s Claude Code gives the handler.
          assert(ms < 4500)
        }
      }
    }
    test(
      "a store that cannot be read gives a start's brief the edge's status, in context and on stderr"
    ) {
      HookFixtures { f =>
        f.script.failing(Some("the database is down"))
        val brief = f.run("brief", f.payload("session-start-startup.json"))
        (brief.exit, printed(brief), brief.err.linesIterator.size, brief.err.contains("503")) ==> (
          0,
          context(
            "SessionStart",
            s"grit: grit's server at ${f.edge} answered 503 when asked for this session's brief, " +
              "so it starts without one."
          ),
          1,
          true
        )
      }
    }
    test(
      "an edge's address the join would refuse makes the hook ask nothing, saying so without the address; holder exits 1"
    ) {
      HookFixtures { f =>
        val refused = Vector(
          "http://192.0.2.7:7317",
          s"${f.edge}/x",
          "ftp://127.0.0.1:7317",
          "https://user:secret@127.0.0.1:7317"
        )
        val runs = refused.map { edge =>
          val env = f.claude + ("GRIT_EDGE" -> edge)
          (
            f.run("brief", f.payload("session-start-startup.json"), env),
            f.run("holder", "", env)
          )
        }
        (
          runs.map((b, h) => (b.exit, b.out, b.err.linesIterator.size, h.exit, h.out)),
          // GRIT_EDGE may hold credentials, so no line repeats it.
          refused.zip(runs).collect {
            case (edge, (b, h)) if b.err.contains(edge) || h.err.contains(edge) => edge
          },
          f.script.called
        ) ==> (refused.map(_ => (0, Vector.empty, 1, 1, Vector.empty)), Vector.empty, Vector.empty)
      }
    }
    test("an edge's address the join would refuse makes event and commit post nothing, saying so") {
      HookFixtures { f =>
        // 127.1 is this machine to curl, but not an address the join's loopback rule takes.
        val env = Map("CLAUDE_CODE_SESSION_ID" -> "s-1", "GRIT_EDGE" -> s"http://127.1:${f.port}")
        val event = f.run("event", f.payload("post-tool-use-read.json"), f.claude ++ env)
        val commit = Bash.run(
          f.proj,
          f.machine.home,
          f.machine.bin,
          Seq(
            "-c",
            "git -c user.name=t -c user.email=t@example.invalid commit -q --allow-empty -m x"
          ),
          env = env
        )
        (
          Vector(event, commit).map(r =>
            (r.exit, r.err.linesIterator.size, r.err.contains("did nothing"))
          ),
          f.recorded
        ) ==> (Vector((0, 1, true), (0, 1, true)), Vector.empty)
      }
    }

    test("input that never ends is stopped at the hook's bound, which exits 0 saying so") {
      HookFixtures { f =>
        val began = System.nanoTime()
        val done = Bash.run(
          f.proj,
          f.machine.home,
          f.machine.bin,
          Seq("-c", s"${Argv.quote(f.hook.toString)} brief < <(exec sleep 30 2> /dev/null)"),
          env = f.claude,
          within = 40.seconds
        )
        val ms = (System.nanoTime() - began) / 1000000
        (done.exit, done.out, done.err.contains("ran past its bound")) ==> (0, Vector.empty, true)
        // The 3 s bound, well short of the input's 30 s.
        assert(ms >= 2900, ms < 10000)
      }
    }
    test("the hook checks an address with the join's own patterns") {
      def patterns(script: String) =
        script.linesIterator
          .filter(l => l.trim.startsWith("address=") || l.trim.matches("(local )?loopback=.*"))
          .map(
            _.trim.stripPrefix("local ")
          )
          .toVector
      (patterns(Scripts.Hook), patterns(Scripts.Hook).size) ==> (patterns(Scripts.Join), 2)
    }
  }
}
