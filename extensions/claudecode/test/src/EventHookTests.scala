package grit.claudecode

import java.nio.file.{Files, Path}
import java.time.Instant

import grit.commands.routes.RouteFixtures.{Worker, got}
import grit.commands.worker.Holders
import grit.core.edge.EdgeSessions
import grit.core.place.Directory
import grit.core.session.{Event, Session}

import utest.*

/** The hook's `event`, `commit` and `rewrite` roles on Claude Code's recorded payloads and real git
  * commits, against the commands edge: what the edge recorded, and that they print nothing.
  */
object EventHookTests extends TestSuite {

  private val First = "11111111-1111-4111-8111-111111111111"
  private val Forked = "22222222-2222-4222-8222-222222222222"

  /** An event as a test compares it: its kind's name, a read's bytes or a closing's summary, and
    * its shape.
    */
  private def seen(e: Event): (String, Option[Long], Option[String], ujson.Value) = (
    e.kind.name,
    e.kind match { case Event.Kind.Read(b) => Some(b); case _ => None },
    e.kind match { case Event.Kind.Closing(s) => Some(Session.Summary.value(s)); case _ => None },
    ujson.read(Event.Shape.value(e.shape))
  )

  /** A batch as a test compares it: its session, parent and holder, and its events as [[seen]]. */
  private def batch(b: EdgeSessions.Batch) =
    (b.id, b.parent, b.holder, b.events.map(seen))

  /** Each of `names`' payloads run through `event`, each printing nothing; the batches recorded. */
  private def events(f: HookFixtures, names: String*): Vector[EdgeSessions.Batch] = {
    val runs = names.map(n => f.run("event", f.payload(n))).toVector
    runs.map(r => (r.exit, r.out, r.err)) ==> runs.map(_ => (0, Vector.empty, ""))
    f.recorded
  }

  private def obj(kv: (String, ujson.Value)*): ujson.Value = ujson.Obj.from(kv)

  /** bash running `script` in `dir` as a Bash tool of the session `First` runs it. */
  private def bashTool(
      f: HookFixtures,
      dir: Path,
      script: String,
      session: Boolean = true
  ): Bash.Done =
    Bash.run(
      dir,
      f.machine.home,
      f.machine.bin,
      Seq("-c", script),
      env = if (session) Map("CLAUDE_CODE_SESSION_ID" -> First) else Map.empty
    )

  private val Git = "git -c user.name=t -c user.email=t@example.invalid"

  val tests = Tests {
    test("each tool's use, and a failed one, records its event in grit's words on its session") {
      HookFixtures { f =>
        val recorded = events(
          f,
          "post-tool-use-read.json",
          "post-tool-use-edit.json",
          "post-tool-use-write.json",
          "post-tool-use-bash.json",
          "post-tool-use-mcp-scout-find.json",
          "post-tool-use-failure-read.json"
        )
        val scouted =
          ujson
            .read(f.payload("post-tool-use-mcp-scout-find.json"))("tool_response")(0)("text")
            .str
            .getBytes("UTF-8")
        val session = (f.session(First), None, Some(f.holderOf()))
        recorded.map(batch) ==> Vector(
          (session, Vector(("read", Some(11L), None, obj("path" -> "a.txt", "ok" -> true)))),
          (session, Vector(("edited", None, None, obj("path" -> "a.txt", "ok" -> true)))),
          (session, Vector(("edited", None, None, obj("path" -> "b.txt", "ok" -> true)))),
          (session, Vector(("ran", None, None, obj("program" -> "git", "ok" -> true)))),
          (
            session,
            Vector(("read", Some(scouted.length.toLong), None, obj("tool" -> "find", "ok" -> true)))
          ),
          (session, Vector(("read", Some(0L), None, obj("path" -> "missing.txt", "ok" -> false))))
        ).map { case ((id, parent, holder), es) => (id, parent, holder, es) }
      }
    }
    test("an agent's tool use, start and stop are recorded on the agent, as its type's holder") {
      HookFixtures { f =>
        val recorded = events(
          f,
          "post-tool-use-bash-agent.json",
          "subagent-start-worktree.json",
          "subagent-stop.json"
        )
        recorded.map(batch) ==> Vector(
          (
            got(Holders.agentSession(f.session(Forked), "a2222222222222222")),
            Some(f.session(Forked)),
            Some(f.holderOf(Some("general-purpose" -> Forked))),
            Vector(("ran", None, None, obj("program" -> "printenv", "ok" -> true)))
          ),
          (
            got(Holders.agentSession(f.session(First), "a1111111111111111")),
            Some(f.session(First)),
            Some(f.holderOf(Some("general-purpose" -> First))),
            Vector(("started", None, None, obj()))
          ),
          (
            got(Holders.agentSession(f.session(First), "a1111111111111111")),
            Some(f.session(First)),
            Some(f.holderOf(Some("general-purpose" -> First))),
            Vector(("ended", None, None, obj()))
          )
        )
      }
    }
    test(
      "a session's start records its source, a stop what it last said, a compaction its summary and trigger, an end its reason"
    ) {
      HookFixtures { f =>
        val recorded =
          events(
            f,
            "session-start-fork.json",
            "stop.json",
            "post-compact.json",
            "session-end-other.json"
          )
        recorded.map(b => (b.id, b.events.map(seen))) ==> Vector(
          f.session(Forked) -> Vector(("started", None, None, obj("source" -> "fork"))),
          f.session(First) -> Vector(("closing", None, Some("done"), obj())),
          f.session(First) -> Vector(
            ("closing", None, Some("Steps done; agent returned ok."), obj("trigger" -> "manual"))
          ),
          f.session(First) -> Vector(("ended", None, None, obj("reason" -> "other")))
        )
      }
    }
    test(
      "a stop with nothing said, a compaction's agent no start announced, an unmapped tool and a PreCompact record nothing"
    ) {
      HookFixtures { f =>
        val silent = f
          .payload("stop.json")
          .replace("\"last_assistant_message\": \"done\"", "\"last_assistant_message\": \"\"")
        val runs = Vector(
          f.run("event", silent),
          f.run("event", f.payload("subagent-stop-compaction.json")),
          f.run("event", f.payload("post-tool-use-agent.json")),
          f.run("event", f.payload("pre-compact.json"))
        )
        (runs.map(r => (r.exit, r.out, r.err)), f.script.called) ==> (
          runs.map(_ => (0, Vector.empty, "")),
          Vector.empty
        )
      }
    }
    test("each event's ordinal is above the one before, and its at is the moment it ran") {
      HookFixtures { f =>
        val before = Instant.now()
        val recorded =
          events(f, "session-start-startup.json", "post-tool-use-read.json", "stop.json")
        val after = Instant.now()
        val stamped = recorded.flatMap(_.events)
        (
          stamped.map(_.n).sliding(2).forall { case Seq(a, b) => a < b; case _ => false },
          stamped.forall(e => !e.at.isBefore(before.minusMillis(1)) && !e.at.isAfter(after)),
          stamped.size
        ) ==> (true, true, 3)
      }
    }
    test(
      "quotes, $(…), newlines and a megabyte in a payload reach the edge as data, cut to the line's bounds"
    ) {
      HookFixtures { f =>
        val hostile = "\"'; $(touch pwned) `touch pwned`\n\\"
        val mega = hostile * ((1 << 20) / hostile.length)
        def withField(name: String, key: String, value: String) = {
          val p = ujson.read(f.payload(name))
          p(key) = value
          ujson.write(p)
        }
        def withInput(name: String, key: String, value: String) = {
          val p = ujson.read(f.payload(name))
          p("tool_input")(key) = value
          ujson.write(p)
        }
        val read = ujson.read(f.payload("post-tool-use-read.json"))
        read("tool_response")("file")("content") = mega
        read("tool_input")("file_path") = s"${f.proj}/it's $$(x)\nname.txt"
        val recorded = events(f)
        val runs = Vector(
          f.run("event", withInput("post-tool-use-bash.json", "command", "touch" + mega)),
          f.run("event", withField("stop.json", "last_assistant_message", mega)),
          f.run("event", ujson.write(read)),
          f.run("event", withField("post-compact.json", "compact_summary", "😀" * 1500))
        )
        val said = f.recorded.flatMap(_.events).map(seen)
        (
          recorded,
          runs.map(r => (r.exit, r.out, r.err)),
          said.map((kind, bytes, text, shape) => (kind, bytes, text.map(_.length), shape)),
          said(1)._3.exists(t => mega.startsWith(t)),
          Files.exists(f.proj.resolve("pwned"))
        ) ==> (
          Vector.empty,
          runs.map(_ => (0, Vector.empty, "")),
          Vector(
            ("ran", None, None, obj("program" -> "touch\"';", "ok" -> true)),
            ("closing", None, Some(2000), obj()),
            (
              "read",
              Some(mega.getBytes("UTF-8").length.toLong),
              None,
              obj("path" -> "it's $(x)?name.txt", "ok" -> true)
            ),
            ("closing", None, Some(2000), obj("trigger" -> "manual"))
          ),
          true,
          false
        )
      }
    }
    test(
      "a commit in a session's Bash records its sha, subject and worktree; outside a session or mid-rebase, none"
    ) {
      HookFixtures { f =>
        val made = bashTool(
          f,
          f.proj,
          s"$Git commit -q --allow-empty -m 'Add the notes' && git rev-parse HEAD"
        )
        val outside =
          bashTool(f, f.proj, s"$Git commit -q --allow-empty -m outside", session = false)
        val rebasing = bashTool(
          f,
          f.proj,
          s"mkdir .git/rebase-merge && $Git commit -q --allow-empty -m mid && rmdir .git/rebase-merge"
        )
        val sha = made.out.lastOption.getOrElse("")
        val recorded = f.recorded
        val shape = recorded.flatMap(_.events).map(e => ujson.read(Event.Shape.value(e.shape)))
        (
          (made.exit, made.err, outside.exit, outside.err, rebasing.exit, rebasing.err),
          recorded.map(b => (b.id, b.holder, b.events.map(_.kind.name))),
          shape.map(s =>
            (s("sha").str, s("subject").str, s("worktree").str.endsWith(f.proj.toString))
          )
        ) ==> (
          (0, "", 0, "", 0, ""),
          Vector((f.session(First), Some(f.holderOf()), Vector("commit"))),
          Vector((sha, "Add the notes", true))
        )
      }
    }
    test(
      "a rebase records the commits it left in order, each with the ones it replaces, in one post"
    ) {
      HookFixtures { f =>
        val made = bashTool(
          f,
          f.proj,
          s"$Git commit -q --allow-empty -m one && $Git commit -q --allow-empty -m two && git rev-parse HEAD~1 HEAD" +
            // A commit made again in the same second is the same commit: the rebase dates its own.
            s" && GIT_COMMITTER_DATE=2001-01-01T00:00:00Z $Git rebase -q --force-rebase HEAD~2 && git rev-parse HEAD~1 HEAD"
        )
        val (olds, news) = made.out.splitAt(2)
        val rewritten = f.recorded.drop(2)
        (
          made.exit,
          rewritten
            .map(b => b.events.map(e => ujson.read(Event.Shape.value(e.shape))))
            .map(
              _.map(s => (s("sha").str, s("subject").str, s("replaces").arr.map(_.str).toVector))
            ),
          rewritten.flatMap(_.events.map(_.n)).sliding(2).forall {
            case Seq(a, b) => b == a + 1; case _ => false
          }
        ) ==> (
          0,
          Vector(
            Vector(
              (news(0), "one", Vector(olds(0).take(12))),
              (news(1), "two", Vector(olds(1).take(12)))
            )
          ),
          true
        )
      }
    }
    test("an amend records the commit as made, and again as replacing the one it rewrote") {
      HookFixtures { f =>
        val made = bashTool(
          f,
          f.proj,
          s"$Git commit -q --allow-empty -m draft && git rev-parse HEAD && $Git commit -q --amend --allow-empty -m final && git rev-parse HEAD"
        )
        val shapes = f.recorded.flatMap(_.events).map(e => ujson.read(Event.Shape.value(e.shape)))
        (
          made.exit,
          shapes.map(s =>
            (s("sha").str, s("subject").str, s.obj.get("replaces").map(_.arr.map(_.str).toVector))
          )
        ) ==> (
          0,
          Vector(
            (made.out(0), "draft", None),
            (made.out(1), "final", None),
            (made.out(1), "final", Some(Vector(made.out(0).take(12))))
          )
        )
      }
    }
    test(
      "a post the edge cannot write is dropped once, saying its status, and the tool's use goes on"
    ) {
      HookFixtures { f =>
        f.script.failing(Some("the database is down"))
        val run = f.run("event", f.payload("post-tool-use-read.json"))
        (
          run.exit,
          run.out,
          run.err.linesIterator.size,
          run.err.contains("503"),
          f.recorded.size
        ) ==> (0, Vector.empty, 1, true, 1)
      }
    }
    test(
      "with the edge's port closed, an event and a commit end at once, each saying so in one line"
    ) {
      HookFixtures { f =>
        val edge = s"http://127.0.0.1:${HookFixtures.closedPort}"
        val (run, ms) =
          f.timed("event", f.payload("post-tool-use-read.json"), f.claude + ("GRIT_EDGE" -> edge))
        val made = Bash.run(
          f.proj,
          f.machine.home,
          f.machine.bin,
          Seq("-c", s"$Git commit -q --allow-empty -m down"),
          env = Map("CLAUDE_CODE_SESSION_ID" -> First, "GRIT_EDGE" -> edge)
        )
        (
          (run.exit, run.out, run.err.linesIterator.size),
          (made.exit, made.err.linesIterator.size)
        ) ==> ((0, Vector.empty, 1), (0, 1))
        assert(ms < 1000)
      }
    }
    test("a proxy the environment names is never asked for a loopback edge") {
      HookFixtures { f =>
        HookFixtures.holding { (port, asked) =>
          val proxy = s"http://127.0.0.1:$port"
          val env = f.claude ++ Vector("http_proxy", "HTTP_PROXY", "all_proxy", "ALL_PROXY")
            .map(_ -> proxy)
          val brief = f.run("brief", f.payload("session-start-startup.json"), env)
          val event = f.run("event", f.payload("post-tool-use-read.json"), env)
          (
            (brief.exit, brief.err, event.exit, event.err),
            f.briefed.size,
            f.recorded.size,
            asked()
          ) ==> ((0, "", 0, ""), 1, 1, 0)
        }
      }
    }

    test("with an edge that never answers, a session's end ends within its 1.2 s bound") {
      HookFixtures { f =>
        HookFixtures.silent { port =>
          val env = f.claude + ("GRIT_EDGE" -> s"http://127.0.0.1:$port")
          val (run, ms) = f.timed("event", f.payload("session-end-other.json"), env)
          (run.exit, run.out, run.err.linesIterator.size) ==> (0, Vector.empty, 1)
          assert(ms < 1700)
        }
      }
    }
    test(
      "a join piped from the edge's own /join installs a hook its settings run, which posts to the edge"
    ) {
      HookFixtures { f =>
        val other = f.machine.checkout("other")
        val joined = Bash.run(
          other,
          f.machine.home,
          f.machine.bin,
          Seq(
            "-c",
            s"curl -fsS ${f.edge}/join/claude-code | bash -s -- ${f.edge} --mcp-name ${f.machine.Name}"
          )
        )
        val command = ujson
          .read(Files.readString(f.machine.settings(other)))("hooks")("PostToolUse")(0)("hooks")(0)(
            "command"
          )
          .str
        val run = Bash.run(
          other,
          f.machine.home,
          f.machine.bin,
          Seq("-c", command),
          f.payload("post-tool-use-edit.json").replace(f.proj.toString, other.toString),
          Map("CLAUDE_PROJECT_DIR" -> other.toString)
        )
        val holder = Holders.of(Worker, got(Directory.of(other.toString)), None)
        (
          joined.exit,
          command,
          (run.exit, run.out, run.err),
          f.recorded.map(b => (b.holder, b.events.map(seen)))
        ) ==> (
          0,
          s"'${f.machine.hook(other)}' event",
          (0, Vector.empty, ""),
          Vector(
            (Some(holder), Vector(("edited", None, None, obj("path" -> "a.txt", "ok" -> true))))
          )
        )
      }
    }
  }
}
