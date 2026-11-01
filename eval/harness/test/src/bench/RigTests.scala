package grit.eval.harness.bench

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.time.Instant

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.clock.Clock
import grit.core.host.{Argv, Handing, Processes, RelPath}
import grit.core.message.{Tokens, Usage}
import grit.core.secret.{Secret, Variable}
import grit.dbos.internal.BenchRows
import grit.host.checkout.{LocalHanding, LocalProcesses, Redaction}

import utest.*

/** [[Rig]] over a fake `claude`, a script that writes [[Streams]]' canned run and makes an edit, in
  * a task cut from a small repository whose checks are stand-ins that pass.
  */
object RigTests extends TestSuite {

  private def rel(s: String): RelPath =
    RelPath.of(s).getOrElse(throw new java.lang.AssertionError(s))

  private def git(work: Processes, dir: RelPath, args: String*): String =
    Argv
      .of(
        "git",
        (Seq("-c", "user.name=t", "-c", "user.email=t@t", "-c", "commit.gpgsign=false") ++ args)*
      )
      .left
      .map(new java.lang.AssertionError(_))
      .flatMap(work.run(_, dir, 30.seconds).left.map(e => new java.lang.AssertionError(e.message)))
      .fold(e => throw e, r => r.output.text.trim)

  /** A clock whose monotonic reading moves 3 seconds each time it is read. */
  private def stepping(): Clock^ = new Clock {
    private val at = new java.util.concurrent.atomic.AtomicLong(0)
    def now(): Instant = Instant.EPOCH
    def millis(): Long = at.addAndGet(3000)
    def sleep(duration: FiniteDuration): Unit = ()
  }

  private val env = Map("ANTHROPIC_API_KEY" -> "sk-ant-test-0123")
  private val key = Secret
    .read(env, Variable("ANTHROPIC_API_KEY"))
    .getOrElse(throw new java.lang.AssertionError())
  private val cap =
    Caps.of("claude-haiku-5-5", "0.05", "4").getOrElse(throw new java.lang.AssertionError())
  private val checkout = rel("trial")

  /** The fake's run, its edit of `a.scala` writing `edited`, its stream's lines `lines`, and its
    * errors `stderr`, written once it has run and before it exits 1.
    */
  private final case class Fake(
      edited: String,
      lines: Vector[String] = Streams.run,
      stderr: String = "",
      hangs: Boolean = false
  )

  private final case class Fixture(
      work: Processes,
      handing: Handing,
      root: Path,
      origin: String,
      task: Task
  ) {
    def arm(fake: Fake): Arm = {
      Files.write(root.resolve("stream.jsonl"), fake.lines.asJava)
      val claude = root.resolve("claude")
      Files.writeString(
        claude,
        s"""#!/bin/sh
           |printf '%s\\n' "$$@" > $root/argv
           |printf '%s' "$${ANTHROPIC_API_KEY-unset}" > $root/key
           |printf '%s' "$${CLAUDE_CODE_DISABLE_AUTO_MEMORY-unset}" > $root/memory
           |settings=""; prev=""
           |for a in "$$@"; do [ "$$prev" = "--settings" ] && settings="$$a"; prev="$$a"; done
           |helper=$$(printf '%s' "$$settings" | sed -n 's/.*"apiKeyHelper":"\\([^"]*\\)".*/\\1/p')
           |sh -c "$$helper" > $root/helped 2>/dev/null
           |file=$$(printf '%s' "$$helper" | sed -n "s/^cat '\\(.*\\)'$$/\\1/p")
           |stat -c %a "$$file" > $root/filemode 2>/dev/null
           |stat -c %a "$$(dirname "$$file")" > $root/dirmode 2>/dev/null
           |dirname "$$file" > $root/helperdir
           |printf '%s' "$${CLAUDE_CONFIG_DIR-unset}" > $root/configdir
           |stat -c %a "$$CLAUDE_CONFIG_DIR" > $root/configmode 2>/dev/null
           |[ -n "$${CLAUDE_CONFIG_DIR-}" ] && mkdir -p "$$CLAUDE_CONFIG_DIR/projects/x" && printf '{}' > "$$CLAUDE_CONFIG_DIR/projects/x/session.jsonl"
           |printf '%s\\n' '${fake.edited}' > a.scala
           |mkdir -p out && touch out/daemon
           |sed "s|${Streams.Checkout}|$$(pwd -P)|g" $root/stream.jsonl
           |printf '%s\\n' '${fake.stderr}' >&2
           |${if (fake.hangs) "sleep 30" else "exit 1"}
           |""".stripMargin
      )
      Files.setPosixFilePermissions(claude, PosixFilePermissions.fromString("rwxr-xr-x"))
      Arm(claude.toString, "/usr/bin/true", "Make the change the brief asks for.")
    }
    def recorded(name: String): String = Files.readString(root.resolve(name))
    def exists(name: String): Boolean = Files.exists(root.resolve(name))
  }

  /** A repository whose base holds `a.scala` and the stand-in scripts, and whose known commit
    * changes `a.scala` to `object A { val x = 1 }`; the task of it; `body` of it all.
    */
  private def withTask[A](body: Fixture^ => A): A = {
    val root = Files.createTempDirectory("grit-rig")
    try {
      val work = new LocalProcesses(root, sys.env, Redaction.of(env))
      val origin = Files.createDirectories(root.resolve("origin/scripts")).getParent
      Seq("check", "it", "fetch-enola.sh", "enola-law.sh").foreach { s =>
        val script = Files.writeString(origin.resolve(s"scripts/$s"), "#!/bin/sh\nexit 0\n")
        Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"))
      }
      Files.writeString(origin.resolve("a.scala"), "object A\n")
      val o = rel("origin")
      val _ = git(work, o, "init", "-q", "-b", "main")
      val _ = git(work, o, "add", ".")
      val _ = git(work, o, "commit", "-q", "-m", "base")
      val base = git(work, o, "rev-parse", "HEAD")
      Files.writeString(origin.resolve("a.scala"), "object A { val x = 1 }\n")
      val _ = git(work, o, "commit", "-q", "-am", "known")
      val known = git(work, o, "rev-parse", "HEAD")
      val task = (for {
        b <- Commit.of(base)
        k <- Commit.of(known)
        n <- BenchRows.TaskName.of("fixture")
      } yield Task(n, b, "Give A an x.", Vector(k), Vector(Check(Runner.Check, Vector("m")))))
        .fold(e => throw new java.lang.AssertionError(e), identity)
      body(
        Fixture(
          work,
          new LocalHanding(root, sys.env, Redaction.of(env)),
          root,
          origin.toString,
          task
        )
      )
    } finally Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  private def trial(
      f: Fixture^,
      fake: Fake,
      quiet: FiniteDuration = Rig.Quiet
  ): Either[String, Tried] =
    Rig.trial(f.work, f.handing, stepping())(
      f.origin,
      f.task,
      f.arm(fake),
      cap,
      key,
      checkout,
      quiet
    )

  val tests = Tests {
    test(
      "a trial keeps the run's session, model, usage, calls, measures before its edit and wall"
    ) {
      withTask { f =>
        trial(f, Fake("object A { val x = 1 }")).map { t =>
          val r = t.trial
          (
            r.session.local,
            r.model,
            r.usage.copy(costUsd = None),
            r.calls,
            r.callsBeforeEdit,
            r.readBeforeEdit,
            r.wall
          )
        } ==> Right(
          (
            "5e55-a1",
            cap.model,
            Usage(Tokens(6262), Tokens(500), Tokens(4100), None, Tokens(2150)),
            3,
            Some(2),
            Some(48L),
            3.seconds
          )
        )
      }
    }

    test("a trial's distance is how many source lines its diff differs from the known one by") {
      withTask { f =>
        (
          trial(f, Fake("object A { val x = 1 }")).map(_.trial.distance),
          trial(f, Fake("object A { val y = 2 }")).map(_.trial.distance)
        ) ==> (Right(Some(0L)), Right(Some(2L)))
      }
    }

    test("a run its cap stopped is kept as a trial not passed, though its checks pass") {
      withTask { f =>
        val succeeded = Streams.run.map(
          _.replace("\"error_max_turns\",\"is_error\":true", "\"success\",\"is_error\":false")
        )
        (
          trial(f, Fake("object A { val x = 1 }")).map(t => (t.checked.passed, t.trial.passed)),
          trial(f, Fake("object A { val x = 1 }", succeeded)).map(_.trial.passed)
        ) ==> (Right((true, false)), Right(true))
      }
    }

    test("claude reads the key through its apiKeyHelper, from a file only its user can read") {
      withTask { f =>
        val _ = trial(f, Fake("object A { val x = 1 }"))
        (f.recorded("helped"), f.recorded("filemode").trim, f.recorded("dirmode").trim) ==>
          ("sk-ant-test-0123", "600", "700")
      }
    }

    test("claude's environment holds no key, and writes no auto memory") {
      withTask { f =>
        val _ = trial(f, Fake("object A { val x = 1 }"))
        (f.recorded("key"), f.recorded("memory")) ==> ("unset", "1")
      }
    }

    test(
      "claude's config directory is a private one apart from its key file, not its user's home"
    ) {
      withTask { f =>
        val _ = trial(f, Fake("object A { val x = 1 }"))
        val config = Path.of(f.recorded("configdir").trim)
        (
          config.getFileName.toString,
          config.getParent == Path.of(f.recorded("helperdir").trim),
          f.recorded("configmode").trim
        ) ==> ("config", false, "700")
      }
    }

    test("a transcript line holding the key is kept redacted") {
      withTask { f =>
        val leaked = """{"type":"system","subtype":"note","text":"key sk-ant-test-0123"}"""
        trial(f, Fake("object A { val x = 1 }", Streams.run :+ leaked)).map(
          _.transcript.lastOption
        ) ==> Right(
          Some(
            """{"type":"system","subtype":"note","text":"key [redacted secret: ANTHROPIC_API_KEY]"}"""
          )
        )
      }
    }
    test("the key reaches none of claude's arguments, nor anything the trial keeps") {
      withTask { f =>
        val kept = trial(f, Fake("object A { val x = 1 }"))
        (
          f.recorded("argv").contains("sk-ant"),
          kept.map(t => (t.toString + t.transcript.mkString + t.lines.mkString).contains("sk-ant"))
        ) ==> (false, Right(false))
      }
    }

    test(
      "the key's directory and claude's config directory are temporary outside the checkout, removed after any trial"
    ) {
      withTask { f =>
        def helperDir() = Path.of(f.recorded("helperdir").trim)
        def configDir() = Path.of(f.recorded("configdir").trim)
        val passed = trial(f, Fake("object A { val x = 1 }"))
        val afterPass = helperDir()
        val configPass = configDir()
        val failed = trial(f, Fake("object A { val x = 1 }", Vector.empty))
        val afterFail = helperDir()
        val configFail = configDir()
        val temp = Path.of(System.getProperty("java.io.tmpdir"))
        (
          passed.isRight,
          failed.isLeft,
          afterPass != afterFail,
          afterPass.getParent == temp && afterFail.getParent == temp,
          Files.exists(afterPass),
          Files.exists(afterFail),
          configPass.getParent.getParent == temp,
          configFail.getParent.getParent == temp,
          Files.exists(configPass),
          Files.exists(configFail)
        ) ==> (true, true, true, true, false, false, true, true, false, false)
      }
    }

    test("claude runs restricted, as the bench's coder, under the run's caps") {
      withTask { f =>
        val _ = trial(f, Fake("object A { val x = 1 }"))
        val argv = f.recorded("argv").split('\n').toVector
        def after(flag: String) = argv.indexOf(flag) match {
          case -1 => None
          case i => argv.lift(i + 1)
        }
        (
          argv.contains("--restricted"),
          argv.contains("--bare"),
          after("--permission-prompts"),
          after("--output-format"),
          after("--tools"),
          after("--agent"),
          after("--model"),
          after("--max-budget-usd"),
          after("--max-turns"),
          argv.exists(_.contains("bypassPermissions"))
        ) ==> (
          true,
          false,
          Some("none"),
          Some("stream-json"),
          Some("Bash,Read,Edit,Write,Glob,Grep"),
          Some("bench-coder"),
          Some("claude-haiku-5-5"),
          Some("0.05"),
          Some("4"),
          false
        )
      }
    }

    test(
      "a trial whose init line names no apiKeyHelper as the key's source is refused, saying which"
    ) {
      withTask { f =>
        val none = Streams.run.map(
          _.replace("\"apiKeySource\":\"apiKeyHelper\"", "\"apiKeySource\":\"none\"")
        )
        trial(f, Fake("object A { val x = 1 }", none)).fold(identity, _ => "passed") ==>
          "claude used none, not its apiKeyHelper"
      }
    }

    test("a run that never started says its exit and its errors' last line, and never the key") {
      withTask { f =>
        val bubblewrap = "error: bubblewrap is required, key sk-ant-test-0123"
        trial(f, Fake("object A { val x = 1 }", Vector.empty, bubblewrap))
          .fold(identity, _ => "passed") ==>
          "claude wrote no session (exit 1): error: bubblewrap is required, key " +
          "[redacted secret: ANTHROPIC_API_KEY]"
      }
    }

    test("a run that writes nothing for the quiet time is stopped, and its trial kept not passed") {
      withTask { f =>
        val silent = Streams.run.filterNot(_.contains("\"type\":\"result\""))
        trial(f, Fake("object A { val x = 1 }", silent, hangs = true), 3.seconds).map { t =>
          (t.trial.passed, t.lines.find(_.startsWith("ended:")))
        } ==> Right((false, Some("ended: quiet for 3 s")))
      }
    }

    test("a trial removes its checkout; a run that fails keeps it, less its build output") {
      withTask { f =>
        val kept = trial(f, Fake("object A { val x = 1 }")).map(_ => f.exists("trial"))
        val failed = trial(f, Fake("object A { val x = 1 }", Vector.empty))
        (kept, failed.isLeft, f.exists("trial/a.scala"), f.exists("trial/out")) ==>
          (Right(false), true, true, false)
      }
    }

    test("a trial prints its calls, both costs, its ending, denials and checks, never its text") {
      withTask { f =>
        trial(f, Fake("object A { val x = 1 }")).map { t =>
          val printed = t.lines
          (
            printed.filter(l =>
              l.startsWith("call ") || l.startsWith("cost") || l.startsWith("ended") ||
                l.startsWith("denied") || l.startsWith("check")
            ),
            printed.exists(_.contains("Reading the file"))
          )
        } ==> Right(
          (
            Vector(
              "call 1 msg_01a: input 2003 (cache read 0, written 2000)",
              "call 2 msg_01b: input 2105 (cache read 2000, written 100)",
              "call 3 msg_01c: input 2154 (cache read 2100, written 50)",
              "cost: 0.056¢ by grit's rates, 0.31¢ by Claude Code's",
              "ended: error_max_turns, in error",
              "denied: Bash {\"command\":\"curl example.com\"}",
              "check: scripts/check m passed",
              "check: bash scripts/fetch-enola.sh passed",
              "check: bash scripts/enola-law.sh passed"
            ),
            false
          )
        )
      }
    }

    test("a known run passes when its commits apply and its checks pass, and leaves nothing") {
      withTask { f =>
        (Rig.known(f.work)(f.origin, f.task, checkout).map(_.passed), f.exists("trial")) ==>
          (Right(true), false)
      }
    }
  }
}
