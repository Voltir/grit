package grit.claudecode

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import utest.*

/** The join script ([[ClaudeCode.adapter]]'s `join`), piped into bash in temporary checkouts with
  * a `HOME` of their own and a fake `claude`: what it writes, refuses, prints and undoes.
  */
object JoinTests extends TestSuite {

  private def resource(name: String): String =
    new String(getClass.getResourceAsStream(name).readAllBytes(), UTF_8)

  /** `dir`'s settings with the hook's path written `/HOOK`. */
  private def settingsText(f: JoinFixtures, dir: Path): Option[String] =
    f.read(f.settings(dir)).map(_.replace(f.hook(dir).toString, "/HOOK"))

  /** Settings holding a foreign handler on each side of two a retired directory's program makes. */
  private def foreign(retired: Path): String =
    s"""{
       |  "model": "x",
       |  "hooks": {
       |    "SessionStart": [
       |      {"hooks": [{"type": "command", "command": "/opt/mine/start.sh"}]},
       |      {"hooks": [{"type": "command", "command": "$retired/brief-hook.sh --digest", "timeout": 5}]}
       |    ],
       |    "Stop": [
       |      {"hooks": [{"type": "command", "command": "$retired/spool-hook.sh"}, {"type": "command", "command": "/opt/mine/stop.sh"}]}
       |    ]
       |  }
       |}
       |""".stripMargin

  val tests = Tests {
    test("a bare checkout's settings, once joined, are the golden settings") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val done = f.joined(dir)
        (done.exit, settingsText(f, dir)) ==> ((0, Some(resource("/settings/joined.json"))))
      }
    }

    test("a second join of a checkout changes no byte of what the first wrote") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val _ = f.joined(dir)
        val first = f.tree(dir)
        val again = f.joined(dir)
        (again.exit, first.contains(".claude/settings.local.json"), f.tree(dir) == first) ==>
          ((0, true, true))
      }
    }

    test("the join prints where it joined, what it wrote, and how to undo it") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val done = f.joined(dir)
        val (top, h, hooks) = (dir.toRealPath(), f.hook(dir), f.common(dir).resolve("hooks"))
        (done.exit, done.out, done.err) ==> (
          (
            0,
            Vector(
              s"grit claude-code: joined $top to ${f.Edge}",
              s"  hook      $h, shared by this repository's worktrees",
              "  settings  .claude/settings.local.json: 11 handlers of ours",
              s"  git       post-commit and post-rewrite in $hooks, shared by this repository's worktrees",
              s"  mcp       ${f.Name} at ${f.Edge}/mcp, local scope",
              s"Sessions pick the hooks up at once; run /mcp in a running session to connect ${f.Name}.",
              s"Undo: curl -fsS ${f.Edge}/join/claude-code | bash -s -- --undo --mcp-name ${f.Name}"
            ),
            ""
          )
        )
      }
    }

    test(
      "the hook is installed executable, ahead of the hook script, at an address GRIT_EDGE overrides"
    ) {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val _ = f.joined(dir)
        val h = f.hook(dir)
        (Files.isExecutable(h), f.read(h)) ==> (
          (
            true,
            Some(
              "#!/usr/bin/env bash\nharness='claude-code'\nedge='http://127.0.0.1:7317'\n" +
                "edge=${GRIT_EDGE:-$edge}\n" + Scripts.Hook
            )
          )
        )
      }
    }

    test("git's post-commit and post-rewrite run the hook, marked as the join's") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val _ = f.joined(dir)
        val q = s"'${f.hook(dir)}'"
        (f.read(f.gitHook(dir, "post-commit")), f.read(f.gitHook(dir, "post-rewrite"))) ==> (
          (
            Some(s"#!/usr/bin/env bash\n# grit claude-code join\nexec $q commit\n"),
            Some(s"#!/usr/bin/env bash\n# grit claude-code join\nexec $q rewrite \"$$@\"\n")
          )
        )
      }
    }

    test(
      "foreign handlers stay in their order, and each a retired directory's program makes goes, printed"
    ) {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val retired = Files.createDirectories(f.root.resolve("old-hooks")).toRealPath()
        val _ = f.write(f.settings(dir), foreign(retired))
        val done = f.joined(dir, "--retire", retired.toString)
        val kept = f.sh(
          dir,
          "jq -c '[.model, [.hooks[][].hooks[].command | select(startswith(\"/opt\"))]]' .claude/settings.local.json"
        )
        val ours = f.sh(
          dir,
          "jq '[.hooks[][].hooks[].command | select(contains(\"old-hooks\"))] | length' .claude/settings.local.json"
        )
        (done.exit, kept, ours, done.out.filter(_.startsWith("  removed"))) ==> (
          (
            0,
            Vector("""["x",["/opt/mine/start.sh","/opt/mine/stop.sh"]]"""),
            Vector("0"),
            Vector(
              s"  removed   $retired/brief-hook.sh --digest",
              s"  removed   $retired/spool-hook.sh"
            )
          )
        )
      }
    }

    test(
      "a foreign post-commit is left as it is, with the line to add printed; a retired one is replaced"
    ) {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val retired = Files.createDirectories(f.root.resolve("old-hooks")).toRealPath()
        val mine = "#!/bin/sh\nmake lint\n"
        val _ = f.write(f.gitHook(dir, "post-commit"), mine, executable = true)
        val _ = f.write(
          f.gitHook(dir, "post-rewrite"),
          s"#!/usr/bin/env bash\nexec $retired/spool-hook.sh --rewrite \"$$@\"\n",
          executable = true
        )
        val done = f.joined(dir, "--retire", retired.toString)
        val hooks = f.common(dir).resolve("hooks")
        (
          f.read(f.gitHook(dir, "post-commit")),
          f.read(f.gitHook(dir, "post-rewrite")).exists(_.contains("# grit claude-code join")),
          done.out.filter(l => l.startsWith("  git") || l.startsWith("  removed"))
        ) ==> (
          (
            Some(mine),
            true,
            Vector(
              s"  removed   post-rewrite: exec $retired/spool-hook.sh --rewrite \"$$@\"",
              s"  git       post-rewrite in $hooks, shared by this repository's worktrees",
              s"  git       $hooks/post-commit is not ours and is left as it is; add the line: '${f.hook(dir)}' commit"
            )
          )
        )
      }
    }

    test("settings that are not a JSON object of hook lists are refused, with nothing changed") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val bad = Vector("not json", "[1]", """{"hooks": []}""", """{"hooks": {"Stop": {}}}""")
        val refused = bad.map { text =>
          val _ = f.write(f.settings(dir), text)
          val done = f.joined(dir)
          (
            done.exit,
            done.err.linesIterator.size,
            f.read(f.settings(dir)),
            Files.exists(f.hook(dir))
          )
        }
        refused ==> bad.map(text => (1, 1, Some(text), false))
      }
    }

    test("settings that are a symbolic link are refused, with nothing changed") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val target = f.write(f.root.resolve("elsewhere.json"), "{}")
        val _ = Files.createDirectories(dir.resolve(".claude"))
        val _ = Files.createSymbolicLink(f.settings(dir), target)
        val done = f.joined(dir)
        (done.exit, f.read(target), Files.exists(f.hook(dir))) ==> ((1, Some("{}"), false))
      }
    }

    test("undo leaves settings as they were but the retired handlers, and no file of the join's") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val retired = Files.createDirectories(f.root.resolve("old-hooks")).toRealPath()
        val _ = f.write(f.settings(dir), foreign(retired))
        val _ = f.joined(dir, "--retire", retired.toString)
        val undo = f.undone(dir)
        val left = f.sh(dir, "jq -c . .claude/settings.local.json")
        (
          undo.exit,
          left,
          Files.exists(f.hook(dir).getParent),
          Files.exists(f.gitHook(dir, "post-commit")),
          Files.exists(f.gitHook(dir, "post-rewrite"))
        ) ==> (
          (
            0,
            Vector(
              """{"model":"x","hooks":{"SessionStart":[{"hooks":[{"type":"command","command":"/opt/mine/start.sh"}]}],""" +
                """"Stop":[{"hooks":[{"type":"command","command":"/opt/mine/stop.sh"}]}]}}"""
            ),
            false,
            false,
            false
          )
        )
      }
    }

    test("undo prints what it removed, and that nothing retired was restored") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val _ = f.joined(dir)
        val undo = f.undone(dir)
        (undo.exit, undo.out) ==> (
          (
            0,
            Vector(
              s"grit claude-code: undid the join in ${dir.toRealPath()}",
              "  settings  .claude/settings.local.json: 11 handlers of ours removed",
              s"  hook      ${f.hook(dir)} removed",
              "  git       post-commit and post-rewrite removed",
              s"  mcp       ${f.Name} removed from local scope",
              "Nothing retired was restored."
            )
          )
        )
      }
    }

    test(
      "claude is asked to remove the entry, then to add it at the edge's /mcp; undo removes it"
    ) {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val _ = f.joined(dir)
        val _ = f.undone(dir)
        f.claudeCalls ==> Vector(
          s"mcp remove --scope local ${f.Name}",
          s"mcp add --transport http --scope local ${f.Name} ${f.Edge}/mcp",
          s"mcp remove --scope local ${f.Name}"
        )
      }
    }

    test("a replaced MCP entry is printed with where it pointed") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val top = dir.toRealPath().toString
        val _ = f.write(
          f.home.resolve(".claude.json"),
          s"""{"projects":{"$top":{"mcpServers":{"${f.Name}":{"type":"http","url":"http://127.0.0.1:9/mcp"}}}}}"""
        )
        f.joined(dir).out.filter(_.startsWith("  mcp")) ==> Vector(
          s"  mcp       ${f.Name} at ${f.Edge}/mcp, local scope, replacing ${f.Name} at http://127.0.0.1:9/mcp"
        )
      }
    }

    test("an MCP add that fails prints the command to run and exits 1") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val done =
          f.join(dir, Seq(f.Edge, "--mcp-name", f.Name), Map("FAKE_CLAUDE_ADD_FAILS" -> "1"))
        (done.exit, done.err) ==> (
          (
            1,
            s"grit claude-code: claude mcp add failed; run: claude mcp add --transport http --scope local ${f.Name} ${f.Edge}/mcp\n"
          )
        )
      }
    }

    test("with no claude on PATH, the join prints the MCP command to run") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        Files.delete(f.bin.resolve("claude"))
        f.joined(dir).out.filter(_.startsWith("  mcp")) ==> Vector(
          s"  mcp       claude not found; run: claude mcp add --transport http --scope local ${f.Name} ${f.Edge}/mcp"
        )
      }
    }

    // Every byte of each line at the top level (not indented, a comment or a closing brace),
    // where a cut could leave a statement that runs, and every line end; a sweep of every byte
    // would run bash some 16,000 times.
    test(
      "a prefix of the join cut at a line end or inside a top-level line prints and changes nothing"
    ) {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val join = ClaudeCode.adapter.join
        val starts = 0 +: join.indices.filter(i => join(i) == '\n').map(_ + 1)
        val topLevel = starts
          .filter(s => s < join.length && !" \t#}\n".contains(join(s)))
          .flatMap { s =>
            val end = join.indexOf('\n', s)
            (s + 1) to (if (end < 0) join.length else end + 1)
          }
        val cuts = (starts ++ topLevel).filter(_ < join.length).distinct.sorted
        val prefixes = f.root.resolve("prefixes")
        Files.createDirectories(prefixes)
        cuts.foreach(n => f.write(prefixes.resolve(f"$n%06d"), join.take(n)))
        val before = (f.tree(dir), f.tree(f.home))
        val printed = Bash.run(
          dir,
          f.home,
          f.bin,
          Seq(
            "-c",
            s"""for p in '$prefixes'/*; do bash -s -- '${f.Edge}' < "$$p" 2> /dev/null; done; true"""
          ),
          within = 60.seconds
        )
        val after = (f.tree(dir), f.tree(f.home))
        val whole = f.joined(dir)
        (cuts.size > 400, printed.out, after == before, whole.exit, f.tree(dir) == before._1) ==> ((
          true,
          Vector.empty,
          true,
          0,
          false
        ))
      }
    }

    test("nothing the join runs reads the rest of the script it is piped") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val done = Bash.run(
          dir,
          f.home,
          f.bin,
          Seq("-s", "--", f.Edge, "--mcp-name", f.Name),
          ClaudeCode.adapter.join + "\necho still-read\n"
        )
        (done.exit, done.out.lastOption) ==> ((0, Some("still-read")))
      }
    }

    test("an address that is not a scheme, a host and a port is refused, with nothing changed") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val bad = Vector(
          "example.com:7317",
          "https://example.com/",
          "https://example.com/mcp",
          "https://example.com?x=1",
          "https://example.com#x",
          "https://exa mple.com",
          "https://example.com';touch x;'",
          "https://$(touch x)",
          "ftp://example.com",
          "https://user@example.com",
          "https://example.com:7317\nhttps://example.com"
        )
        val before = f.tree(dir)
        bad.map(e => f.join(dir, Seq(e)).exit) ==> bad.map(_ => 1)
        (f.tree(dir) == before, Files.exists(f.hook(dir))) ==> ((true, false))
      }
    }

    test(
      "plain http to a host beyond loopback is refused; loopback's, and https to any host, join"
    ) {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val refused =
          Vector("http://example.com:7317", "http://10.0.0.2", "http://127.0.0.1.example.com")
            .map(e => f.join(dir, Seq(e)).exit)
        val joined = Vector(
          "http://localhost:7317",
          "http://[::1]:7317",
          "http://127.0.0.1",
          "https://example.com"
        )
          .map(e => f.join(dir, Seq(e, "--mcp-name", f.Name)).exit)
        (refused, joined) ==> ((Vector(1, 1, 1), Vector(0, 0, 0, 0)))
      }
    }

    test("a checkout whose path holds a space and a quote joins, and its hook commands run") {
      JoinFixtures { f =>
        val dir = f.checkout("it's a work")
        val done = f.joined(dir)
        val commands = f.sh(
          dir,
          "jq -r '[.hooks[][].hooks[].command] | unique | .[]' .claude/settings.local.json"
        )
        val ran = commands.map(c => Bash.run(dir, f.home, f.bin, Seq("-c", c)).exit)
        val gitHooks = f.sh(
          dir,
          "\"$(git rev-parse --git-path hooks)/post-commit\"; a=$?; \"$(git rev-parse --git-path hooks)/post-rewrite\" amend < /dev/null; echo \"$a $?\""
        )
        (done.exit, commands.size, ran, gitHooks) ==> ((0, 3, Vector(0, 0, 0), Vector("0 0")))
      }
    }

    test("a checkout whose path holds a newline is refused, with nothing changed") {
      JoinFixtures { f =>
        val dir = f.checkout("work\nthere")
        val done = f.joined(dir)
        (done.exit, Files.exists(f.hook(dir)), Files.exists(f.settings(dir))) ==> ((
          1,
          false,
          false
        ))
      }
    }

    test("the hook's path is absolute and the same from a checkout and from its worktree") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val _ = f.sh(dir, s"git worktree add -q '${f.root.resolve("tree")}'")
        val tree = f.root.resolve("tree")
        val _ = f.joined(tree)
        val _ = f.joined(dir)
        val named = Vector(dir, tree).map(d =>
          f.sh(d, "jq -r '.hooks.Stop[0].hooks[0].command' .claude/settings.local.json").mkString
        )
        named ==> Vector.fill(2)(
          s"'${f.common(dir).toRealPath().resolve("grit/claude-code/hook")}' event"
        )
      }
    }

    test("undo in one worktree keeps the shared hook while another worktree's settings name it") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val tree = f.root.resolve("tree")
        val _ = f.sh(dir, s"git worktree add -q '$tree'")
        val _ = f.joined(dir)
        val _ = f.joined(tree)
        val first = f.undone(tree)
        val kept = (Files.exists(f.hook(dir)), Files.exists(f.gitHook(dir, "post-commit")))
        val last = f.undone(dir)
        (
          first.out.filter(l => l.startsWith("  hook") || l.startsWith("  git")),
          kept,
          last.exit,
          Files.exists(f.hook(dir)),
          Files.exists(f.gitHook(dir, "post-commit"))
        ) ==> (
          (
            Vector(
              s"  hook      ${f.hook(dir)} kept: the worktree ${dir.toRealPath()} still names it",
              "  git       post-commit and post-rewrite kept, as the hook is"
            ),
            (true, true),
            0,
            false,
            false
          )
        )
      }
    }

    test("a retired directory that is the root or HOME is refused, with nothing changed") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        Vector("/", f.home.toString).map(d => f.joined(dir, "--retire", d).exit) ==> Vector(1, 1)
        Files.exists(f.hook(dir)) ==> false
      }
    }

    test("outside a git checkout, the join is refused with one line") {
      JoinFixtures { f =>
        val dir = Files.createDirectories(f.root.resolve("plain"))
        val done = f.joined(dir)
        (done.exit, done.err) ==> ((
          1,
          "grit claude-code: not inside a git checkout; run the join in one\n"
        ))
      }
    }

    test("a test's join never reaches a repository enclosing its temporary root") {
      JoinFixtures { f =>
        val _ = f.sh(f.root, "git init -q .")
        val dir = Files.createDirectories(f.root.resolve("plain"))
        (f.joined(dir).exit, Files.exists(f.root.resolve(".git/grit"))) ==> ((1, false))
      }
    }

    test("without jq on PATH, the join is refused with one line") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val tools = Files.createDirectories(f.root.resolve("tools"))
        Vector("bash", "git", "curl").foreach(t =>
          Files.createSymbolicLink(tools.resolve(t), Path.of(s"/usr/bin/$t"))
        )
        val done = Bash.run(
          dir,
          f.home,
          f.bin,
          Seq("-s", "--", f.Edge),
          ClaudeCode.adapter.join,
          Map("PATH" -> tools.toString)
        )
        (done.exit, done.err) ==> ((1, "grit claude-code: needs jq on PATH\n"))
      }
    }

    test("undo says which foreign git hook still names the hook it removed") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val _ = f.write(
          f.gitHook(dir, "post-commit"),
          s"#!/bin/sh\nmake lint\n'${f.hook(dir)}' commit\n",
          executable = true
        )
        val _ = f.joined(dir)
        val hooks = f.common(dir).resolve("hooks")
        f.undone(dir).out.filter(_.startsWith("  git")) ==> Vector(
          "  git       post-rewrite removed",
          s"  git       $hooks/post-commit is not ours and still names ${f.hook(dir)}; remove that line"
        )
      }
    }

    test("settings keep their mode through a join and an undo; settings the join makes are 0644") {
      JoinFixtures { f =>
        import java.nio.file.attribute.PosixFilePermissions.{fromString, toString as shown}
        val (kept, made) = (f.checkout("kept"), f.checkout("made"))
        val _ = f.write(f.settings(kept), "{\"model\": \"x\"}\n")
        val _ = Files.setPosixFilePermissions(f.settings(kept), fromString("rw-------"))
        val mode = (dir: Path) => shown(Files.getPosixFilePermissions(f.settings(dir)))
        val _ = (f.joined(kept), f.joined(made))
        val joined = (mode(kept), mode(made))
        val _ = f.undone(kept)
        (joined, mode(kept)) ==> ((("rw-------", "rw-r--r--"), "rw-------"))
      }
    }

    test("a git whose worktree list has no -z refuses the join and the undo, naming the version") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val _ = f.joined(dir)
        val old = f.write(
          f.root.resolve("old-git/git"),
          "#!/bin/sh\n[ \"$1 $2\" = 'worktree list' ] && case \" $* \" in *' -z '*) " +
            "echo 'error: unknown switch z' >&2; exit 129 ;; esac\n" +
            s"PATH='${Bash.SystemPath}' exec git \"$$@\"\n",
          executable = true
        )
        val path = Map("PATH" -> s"${old.getParent}:${f.bin}:${Bash.SystemPath}")
        val before = (f.tree(dir), f.tree(f.home))
        val runs = Vector(
          f.join(dir, Seq(f.Edge, "--mcp-name", f.Name), path),
          f.join(
            dir,
            Seq("--undo", "--mcp-name", f.Name),
            path
          )
        ).map(d => (d.exit, d.err))
        (runs, (f.tree(dir), f.tree(f.home)) == before) ==> ((
          Vector.fill(2)(
            (1, "grit claude-code: needs git 2.36 or later, whose git worktree list takes -z\n")
          ),
          true
        ))
      }
    }

    test("a jq failing at any one call of a join or an undo leaves everything as it was") {
      JoinFixtures { f =>
        val dir = f.checkout("work")
        val retired = Files.createDirectories(f.root.resolve("old-hooks")).toRealPath()
        val _ = f.write(f.settings(dir), foreign(retired))
        val calls = f.root.resolve("jq-calls")
        val failing = f.write(
          f.root.resolve("failing/jq"),
          "#!/bin/sh\nn=$(($(cat \"$JQ_CALLS\" 2> /dev/null || echo 0) + 1))\n" +
            "echo \"$n\" > \"$JQ_CALLS\"\n[ \"$n\" = \"$JQ_FAILS_AT\" ] && exit 5\n" +
            s"PATH='${Bash.SystemPath}' exec jq \"$$@\"\n",
          executable = true
        )
        val env = (at: Int) =>
          Map(
            "PATH" -> s"${failing.getParent}:${f.bin}:${Bash.SystemPath}",
            "JQ_CALLS" -> calls.toString,
            "JQ_FAILS_AT" -> at.toString
          )
        val joining = Seq(f.Edge, "--mcp-name", f.Name, "--retire", retired.toString)
        val undoing = Seq("--undo", "--mcp-name", f.Name)
        // The calls a run that fails none makes, counted in a twin checkout.
        val twin = f.checkout("twin")
        val _ = f.write(f.settings(twin), foreign(retired))
        def count(args: Seq[String]): Int = {
          val _ = Files.deleteIfExists(calls)
          val _ = f.join(twin, args, env(0))
          f.read(calls).fold(0)(_.trim.toInt)
        }
        val (joinCalls, undoCalls) = (count(joining), count(undoing))
        // The calls whose failure let the run exit 0 or change something.
        def sweep(dir: Path, args: Seq[String], made: Int): Vector[Int] = {
          val before = (f.tree(dir), f.tree(f.home))
          (1 to made).toVector.filter { n =>
            val _ = Files.deleteIfExists(calls)
            f.join(dir, args, env(n)).exit == 0 || (f.tree(dir), f.tree(f.home)) != before
          }
        }
        val joined = f.checkout("joined")
        val _ = f.write(f.settings(joined), foreign(retired))
        val _ = f.join(joined, joining)
        (
          joinCalls > 3,
          sweep(dir, joining, joinCalls),
          undoCalls > 1,
          sweep(joined, undoing, undoCalls)
        ) ==> ((
          true,
          Vector.empty,
          true,
          Vector.empty
        ))
      }
    }
  }
}
