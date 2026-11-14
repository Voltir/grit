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
    test("a session id holding a / or a quote posts nothing, saying so on stderr") {
      HookFixtures { f =>
        val runs = Vector("a/b", "a'b", "a\"b").map { id =>
          val p = ujson.read(f.payload("post-tool-use-read.json"))
          p("session_id") = id
          f.run("event", ujson.write(p))
        }
        (
          runs.map(r => (r.exit, r.out, r.err.linesIterator.size, r.err.contains("no session id"))),
          f.recorded
        ) ==> (runs.map(_ => (0, Vector.empty, 1, true)), Vector.empty)
      }
    }

    test("a commit's subject holding quotes, $(…) and backticks is recorded as it was written") {
      HookFixtures { f =>
        val subject = "It's \"done\" $(touch pwned) `touch pwned` $HOME \\n"
        val message = f.machine.write(f.machine.root.resolve("message"), subject + "\n")
        val made = bashTool(f, f.proj, s"$Git commit -q --allow-empty -F '$message'")
        val shapes =
          f.recorded.flatMap(_.events).map(e => ujson.read(Event.Shape.value(e.shape)))
        (
          made.exit,
          made.err,
          shapes.map(_("subject").str),
          Files.exists(f.proj.resolve("pwned"))
        ) ==>
          ((0, "", Vector(subject), false))
      }
    }

    test(
      "a signed commit's subject is its own under log.showSignature, not the signature's check"
    ) {
      HookFixtures { f =>
        // An SSH key signs a commit with no GPG keyring to make.
        val made = bashTool(
          f,
          f.proj,
          "ssh-keygen -q -t ed25519 -N '' -f ../key > /dev/null && git config log.showSignature true" +
            s" && $Git -c gpg.format=ssh -c user.signingKey=../key.pub commit -q -S --allow-empty -m signed"
        )
        val shapes =
          f.recorded.flatMap(_.events).map(e => ujson.read(Event.Shape.value(e.shape)))
        (made.exit, made.err, shapes.map(_("subject").str)) ==> ((0, "", Vector("signed")))
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
    test(
      "a rewrite of commits whose lines pass one post's bound records every one, in several posts"
    ) {
      HookFixtures { f =>
        val count = 120
        // fast-import makes the commits in one process; each subject is 100 characters of 3 bytes.
        val stream = (1 to count).map { i =>
          val subject = f"$i%03d " + "語" * 120
          val bytes = subject.getBytes(java.nio.charset.StandardCharsets.UTF_8).length
          s"commit refs/heads/many\ncommitter t <t@example.invalid> $i +0000\ndata $bytes\n$subject\n"
        }.mkString
        val made = Bash.run(
          f.proj,
          f.machine.home,
          f.machine.bin,
          Seq("-c", "git fast-import --quiet && git rev-list --reverse refs/heads/many"),
          stream
        )
        val old = "0123456789abcdef0123456789abcdef01234567"
        val rewritten = made.out.map(sha => s"$old $sha").mkString("\n") + "\n"
        // A deep starting directory, so each line holds some 3.5 KB and 120 pass a post's 256 KiB.
        val deep = f.proj.toString + ("/" + "d" * 200) * 15
        val run = f.run(
          "rewrite",
          rewritten,
          Map("CLAUDE_CODE_SESSION_ID" -> First, "CLAUDE_PROJECT_DIR" -> deep),
          args = Seq("rebase")
        )
        val recorded = f.recorded
        (
          (made.exit, made.out.size, run.exit, run.err),
          recorded.size > 1,
          recorded
            .flatMap(_.events)
            .map(e => ujson.read(Event.Shape.value(e.shape))("subject").str.take(3))
        ) ==> ((0, count, 0, ""), true, (1 to count).map(i => f"$i%03d").toVector)
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
      "with the edge's port closed, an event, a commit and a rewrite end at once, each saying in one line what it dropped"
    ) {
      HookFixtures { f =>
        val edge = s"http://127.0.0.1:${HookFixtures.closedPort}"
        val (run, ms) =
          f.timed("event", f.payload("post-tool-use-read.json"), f.claude + ("GRIT_EDGE" -> edge))
        val made = Bash.run(
          f.proj,
          f.machine.home,
          f.machine.bin,
          Seq("-c", s"$Git commit -q --allow-empty -m down && git rev-parse HEAD HEAD~1"),
          env = Map("CLAUDE_CODE_SESSION_ID" -> First, "GRIT_EDGE" -> edge)
        )
        val rewritten = made.out.map(sha => s"$sha $sha").mkString("\n")
        val rewrite = f.run(
          "rewrite",
          rewritten,
          Map("CLAUDE_CODE_SESSION_ID" -> First, "GRIT_EDGE" -> edge),
          args = Seq("rebase")
        )
        val down = s"dropped: the edge at $edge did not answer\n"
        (
          (run.exit, run.out, run.err.linesIterator.size),
          (made.exit, made.err),
          (rewrite.exit, rewrite.err)
        ) ==> (
          (0, Vector.empty, 1),
          (0, s"grit claude-code hook: the commit made was $down"),
          (0, s"grit claude-code hook: the 2 commits made were $down")
        )
        // At once: well short of the 3 s bound a request that waited would reach.
        assert(ms < 2500)
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

    test("a ~/.curlrc changes neither what the hook prints nor what it records") {
      HookFixtures { f =>
        val _ = f.machine.write(f.machine.home.resolve(".curlrc"), "-i\n")
        val holder = f.run("holder", env = Map("CLAUDE_CODE_SESSION_ID" -> First))
        val event = f.run("event", f.payload("post-tool-use-read.json"))
        (
          (holder.exit, holder.out, holder.err),
          (event.exit, event.err),
          f.recorded.flatMap(_.events).map(_.kind.name)
        ) ==> ((0, Vector(f.holderOf().toString), ""), (0, ""), Vector("read"))
      }
    }

    test("with an edge that never answers, a commit and a rewrite end within their 1 s bound") {
      HookFixtures { f =>
        HookFixtures.silent { port =>
          val env = Map("CLAUDE_CODE_SESSION_ID" -> First, "GRIT_EDGE" -> s"http://127.0.0.1:$port")
          val began = System.nanoTime()
          val made = Bash.run(
            f.proj,
            f.machine.home,
            f.machine.bin,
            Seq("-c", s"$Git commit -q --allow-empty -m held && git rev-parse HEAD"),
            env = env
          )
          val commitMs = (System.nanoTime() - began) / 1000000
          val sha = made.out.lastOption.getOrElse("")
          val (rewrite, rewriteMs) = {
            val at = System.nanoTime()
            val run = f.run("rewrite", s"$sha $sha\n", env, args = Seq("rebase"))
            (run, (System.nanoTime() - at) / 1000000)
          }
          (
            (made.exit, made.err.linesIterator.size),
            (rewrite.exit, rewrite.err.linesIterator.size)
          ) ==> ((0, 1), (0, 1))
          // The 1 s bound, with git's own work, short of the 3 s every other role has.
          assert(commitMs < 2200, rewriteMs < 2200)
        }
      }
    }

    test("with an edge that never answers, a session's end ends within its 1.2 s bound") {
      HookFixtures { f =>
        HookFixtures.silent { port =>
          val env = f.claude + ("GRIT_EDGE" -> s"http://127.0.0.1:$port")
          val (run, ms) = f.timed("event", f.payload("session-end-other.json"), env)
          (run.exit, run.out, run.err.linesIterator.size) ==> (0, Vector.empty, 1)
          // The 1.2 s bound, short of the 3 s every other event has.
          assert(ms < 2700)
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
