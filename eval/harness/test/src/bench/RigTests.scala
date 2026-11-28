package grit.eval.harness.bench

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.time.Instant

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.clock.Clock
import grit.core.host.{Argv, Handing, HostError, Memory, Processes, RelPath, Variables}
import grit.core.id.{Harness, ToolCallId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{
  Assignment,
  Catalog,
  CatalogVersion,
  Effort,
  Grade,
  Ladder,
  ModelRef,
  Pinned,
  ReasoningReplay,
  Settings,
  Upstream,
  WireQuirk
}
import grit.core.secret.{TestSecrets, Variable}
import grit.dbos.internal.TrialRows
import grit.host.checkout.{LocalHanding, LocalProcesses, Redaction}
import grit.models.{AnthropicProvider, OpenRouterConfig, Seed}
import grit.turn.TurnRecord

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

  private val env = Map("ANTHROPIC_API_KEY" -> "sk-ant-test-0123", "JEV_API_KEY" -> "jev-test-4567")
  private val key = TestSecrets.of(Variable("ANTHROPIC_API_KEY"), "sk-ant-test-0123")
  private val jev = TestSecrets.of(Variable("JEV_API_KEY"), "jev-test-4567")
  private val cap =
    Caps.of("claude-haiku-5-5", None, "0.05", "4").getOrElse(throw new java.lang.AssertionError())
  private val deepseek = Caps
    .of("deepseek/deepseek-v4.1-flash-20260910", Some("fireworks"), "0.05", "4")
    .getOrElse(throw new java.lang.AssertionError())
  private val checkout = rel("trial")

  /** The pin a grit coder's turn makes its first rung's calls under on `caps`, as `grit work`
    * reads the arm's environment: the seed's policy with the variables the rig sets laid over
    * it, pinned by the seed's catalog under that policy; and that catalog.
    */
  private def armPin(caps: Caps): (Pinned, Catalog) = (for {
    seed <- Seed.catalog
    passed <- Variables.of(Map("PATH" -> "/usr/bin:/bin"))
    standing <- Standing.of("Make the change.")
    arm <- Arm.Grit.of(
      "grit",
      "scout",
      standing,
      "jdbc:postgresql://localhost:5432/grit_bench_coder",
      8192,
      passed,
      Arm.Settings()
    )
    env = Rig
      .gritEnvironment(arm, caps)
      .flatMap(_.split("=", 2) match {
        case Array(k, v) => Some(k -> v)
        case _ => None
      })
      .toMap
    policy <- OpenRouterConfig.policy(env, seed.policy).left.map(_.message)
    catalog = seed.withPolicy(policy)
  } yield (catalog.pin.turn.first, catalog))
    .fold(why => throw new java.lang.AssertionError(why), identity)

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
      task: BenchTask
  ) {
    def arm(fake: Fake): Arm.ClaudeCode = {
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
      Arm.ClaudeCode(claude.toString, "/usr/bin/true", "Make the change the brief asks for.")
    }

    /** A fake grit launcher that records its arguments, its environment and its brief, edits
      * `a.scala`, writes `fake`'s lines, and then hangs or exits 0.
      */
    def gritArm(
        fake: GritFake,
        maxTokens: Int = 8192,
        settings: Arm.Settings = Arm.Settings()
    ): Arm.Grit = {
      val launcher = root.resolve("grit")
      Files.write(root.resolve("grit.out"), fake.lines.asJava)
      // Each line as a Java string literal, every character past ASCII escaped, so the source
      // reads the same whatever its file's encoding.
      val literals = fake.lines.map(l =>
        l.flatMap(c => if (c < 128 && c != '"' && c != '\\') c.toString else f"\\u${c.toInt}%04x")
      )
      Files.writeString(
        root.resolve("Answer.java"),
        s"""class Answer { public static void main(String[] a) {
           |${literals.map(l => s"""System.out.println("$l");""").mkString("\n")}
           |} }""".stripMargin
      )
      val print =
        if (fake.jvm) s""""$${JAVA_HOME}/bin/java" $root/Answer.java""" else s"cat $root/grit.out"
      Files.writeString(
        launcher,
        s"""#!/bin/sh
           |printf '%s\\n' "$$@" > $root/gargv
           |env | sort > $root/genv
           |for a in "$$@"; do brief="$$a"; done
           |cat "$$brief" > $root/gbrief
           |[ -e $root/shellcheck-fetched ] && touch $root/gsaw-shellcheck
           |printf '%s\\n' 'object A { val x = 1 }' > a.scala
           |$print
           |printf 'a log line\\n' >&2
           |${if (fake.hangs) "sleep 30" else "exit 0"}
           |""".stripMargin
      )
      Files.setPosixFilePermissions(launcher, PosixFilePermissions.fromString("rwxr-xr-x"))
      (for {
        passed <- Variables.of(
          Map(
            "PATH" -> sys.env.getOrElse("PATH", "/usr/bin:/bin"),
            "HOME" -> root.toString,
            "JAVA_HOME" -> sys.props("java.home")
          )
        )
        standing <- Standing.of("Make the change.")
        arm <- Arm.Grit.of(
          launcher.toString,
          "/usr/bin/true",
          standing,
          "jdbc:postgresql://localhost:5432/grit_bench_coder",
          maxTokens,
          passed,
          settings
        )
      } yield arm).fold(e => throw new java.lang.AssertionError(e), identity)
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
        // Each records the JVM options it was given, then passes; once `<script>-heavy` exists in
        // the root, it first marks the root `heavy` and waits, as a check holding much would.
        val script = Files.writeString(
          origin.resolve(s"scripts/$s"),
          s"#!/bin/sh\nprintf '%s' \"$${JDK_JAVA_OPTIONS-unset}\" > $root/$s-jvm\n" +
            s"if [ -e $root/$s-heavy ]; then touch $root/heavy; sleep 10; fi\nexit 0\n"
        )
        Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"))
      }
      // A stand-in shellcheck fetch, which marks the root once it has run.
      val _ = Files.writeString(
        origin.resolve("scripts/fetch-shellcheck.sh"),
        s"#!/usr/bin/env bash\ntouch $root/shellcheck-fetched\n"
      )
      // A stand-in Mill: records its arguments, and fails once `mill-fails` exists in the root;
      // once `mill-heavy` does, it first marks the root `heavy` and waits.
      val mill = Files.writeString(
        origin.resolve("mill"),
        s"""#!/bin/sh
           |printf '%s\\n' "$$@" > $root/mill-argv
           |echo compiled
           |if [ -e $root/mill-heavy ]; then touch $root/heavy; sleep 10; fi
           |if [ -e $root/mill-fails ]; then echo broken; exit 1; fi
           |""".stripMargin
      )
      Files.setPosixFilePermissions(mill, PosixFilePermissions.fromString("rwxr-xr-x"))
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
        b <- Sha.of(base)
        k <- Sha.of(known)
        n <- TrialRows.TaskName.of("fixture")
      } yield BenchTask(n, b, "Give A an x.", Vector(k), Vector(Check(Runner.Check, Vector("m")))))
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

  /** The fake grit launcher's output, a line each, whether it hangs once written, and whether a
    * JVM prints the lines, as `grit work` does, in place of the shell.
    */
  private final case class GritFake(
      lines: Vector[String],
      hangs: Boolean = false,
      jvm: Boolean = false
  )

  /** What a small grit run prints: two calls' lines among others. */
  private val gritLines: Vector[String] = Vector(
    "[provider] anthropic/claude-haiku-5-5 @ api.anthropic.com called with 1 message(s)",
    "[provider] anthropic/claude-haiku-5-5 @ api.anthropic.com answered · in 1200 · read 1000 · " +
      "write 150 · out 40 · 0.0054¢",
    "[provider] anthropic/claude-haiku-5-5 @ api.anthropic.com answered · in 300 · read 0 · " +
      "write 0 · out 10 · 0.0035¢",
    "[main] done"
  )

  /** The turn the fake grit coder recorded: a read, then an edit. */
  private val gritTurn = GritTurn(
    "c-7",
    Vector(
      Message.User("Give A an x."),
      Message.Assistant(
        Vector(AssistantBlock.ToolCall(ToolCallId("t1"), "read", ujson.Obj("path" -> "a.scala"))),
        StopReason.ToolUse,
        Usage.Zero,
        "claude-haiku-5-5"
      ),
      Message.ToolResult(ToolCallId("t1"), "object A", isError = false),
      Message.Assistant(
        Vector(AssistantBlock.ToolCall(ToolCallId("t2"), "edit", ujson.Obj("path" -> "a.scala"))),
        StopReason.ToolUse,
        Usage.Zero,
        "claude-haiku-5-5"
      )
    ),
    "SUCCESS",
    "# The turn\nrendered",
    None,
    RanUnder(Right(None), Right(Some(Tokens(450_000))), Right(None))
  )

  /** A fake [[Memory]]: 9 GB held, of 16 GB available, once the root holds `heavy`, which a
    * stand-in script makes; else `coding` when first asked, as the rig asks before anything runs,
    * and once the fake grit has started; nothing held between, while the checkout compiles.
    */
  private def heavyAt(root: Path, coding: Either[HostError, Memory.Held]): Memory =
    new Memory {
      private val asked = new java.util.concurrent.atomic.AtomicBoolean(false)
      def held(dir: RelPath): Either[HostError, Memory.Held] =
        if (Files.exists(root.resolve("heavy"))) Right(Memory.Held(9L << 30, 16L << 30))
        else if (!asked.getAndSet(true) || Files.exists(root.resolve("gargv"))) coding
        else Right(Memory.Held(0L, 16L << 30))
    }

  /** The memory budget every fake trial runs under, below the 9 GB the heavy fakes hold. */
  private val eightGb: MemoryCap =
    MemoryCap.of("8").fold(e => throw new java.lang.AssertionError(e), identity)

  /** A grit coder's trial over the fake launcher, its turn read back as `gritTurn`. */
  private def gritTrial(
      f: Fixture^,
      fake: GritFake,
      caps: Caps = cap,
      turn: GritTurn = gritTurn,
      heldCoding: Either[HostError, Memory.Held] = Right(Memory.Held(0L, 16L << 30)),
      settings: Arm.Settings = Arm.Settings()
  ): Either[String, Tried] =
    Rig.gritTrial(
      f.work,
      f.handing,
      stepping(),
      new CoderTurns { def task(): Either[String, GritTurn] = Right(turn) },
      heavyAt(f.root, heldCoding)
    )(
      f.origin,
      f.task,
      f.gritArm(fake, settings = settings),
      caps,
      eightGb,
      key,
      jev,
      checkout,
      Rig.Quiet,
      f.root.resolve("grit.log")
    )

  private def trial(
      f: Fixture^,
      fake: Fake,
      quiet: FiniteDuration = Rig.Quiet,
      caps: Caps = cap
  ): Either[String, Tried] =
    Rig.trial(f.work, f.handing, stepping())(
      f.origin,
      f.task,
      f.arm(fake),
      caps,
      key,
      checkout,
      quiet,
      f.root.resolve("claude.log")
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

    test(
      "a run that leaves no diff from the task's base is kept not passed, though it succeeded and its checks pass"
    ) {
      withTask { f =>
        val succeeded = Streams.run.map(
          _.replace("\"error_max_turns\",\"is_error\":true", "\"success\",\"is_error\":false")
        )
        trial(f, Fake("object A", succeeded)).map(t =>
          (t.checked.passed, t.consumed.ended.exists(!_.error), t.trial.passed, t.trial.distance)
        ) ==> Right((true, true, false, None))
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

    test(
      "a trial removes its checkout whatever becomes of it, what the coder left saved to its log first"
    ) {
      withTask { f =>
        def saved(log: String) =
          Option.when(f.exists(log))(
            f.recorded(log)
              .linesIterator
              .toVector
              .filter(l =>
                l.startsWith("[rig] the coder's") || l.startsWith("[rig] its diff") ||
                  l.startsWith("+object")
              )
          )
        val kept = trial(f, Fake("object A { val x = 1 }")).map(_ => f.exists("trial"))
        val failed = trial(f, Fake("object A { val x = 1 }", Vector.empty))
        val refused = gritTrial(f, GritFake(Vector("[main] said")))
        val left = Vector(
          "[rig] the coder's commits:",
          "[rig] its diff against the base:",
          "+object A { val x = 1 }"
        )
        (
          kept,
          failed.isLeft,
          refused.isLeft,
          f.exists("trial"),
          saved("claude.log"),
          saved("grit.log")
        ) ==>
          (Right(false), true, true, false, Some(left), Some(left))
      }
    }

    test(
      "a trial prints its work after its calls: tool calls a model call to one decimal, `-` " +
        "with no model call, its edits and failed edits, suite runs and commits"
    ) {
      val eight = Consumed.Work.of(Vector.fill(8)(Consumed.Work.Used(false, false, None)))
      val printed = withTask { f =>
        trial(f, Fake("object A { val x = 1 }")).map { t =>
          t.lines.dropWhile(!_.startsWith("calls:")).drop(1).headOption
        }
      }
      (printed, Tried.work(eight, 3), Tried.work(Consumed.Work.of(Vector.empty), 0)) ==> (
        Right(
          Some(
            "work: 8 tool calls in 3 model calls (2.7 a call); edits: 2, 1 failed; " +
              "suite runs: 1; commits: 1; refused by the machine guard: 0; shell reads scout " +
              "answers: 0 (the rule points at 0); pointed to scout: 0, 0 taken"
          )
        ),
        "work: 8 tool calls in 3 model calls (2.7 a call); edits: 0, 0 failed; suite runs: 0; " +
          "commits: 0; refused by the machine guard: 0; shell reads scout answers: 0 (the rule " +
          "points at 0); pointed to scout: 0, 0 taken",
        "work: 0 tool calls in 0 model calls (- a call); edits: 0, 0 failed; suite runs: 0; " +
          "commits: 0; refused by the machine guard: 0; shell reads scout answers: 0 (the rule " +
          "points at 0); pointed to scout: 0, 0 taken"
      )
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

    test(
      "a grit trial keeps its turn's session, its calls, its measures before the edit and its transcript"
    ) {
      withTask { f =>
        gritTrial(f, GritFake(gritLines)).map { t =>
          (
            (Harness.value(t.trial.session.harness), t.trial.session.local),
            t.trial.calls,
            Tokens.value(t.trial.usage.input),
            t.trial.callsBeforeEdit,
            t.trial.readBeforeEdit,
            t.trial.passed,
            t.transcript
          )
        } ==> Right(
          (
            ("grit", "c-7"),
            2,
            1500L,
            Some(1),
            Some(8L),
            true,
            gritLines ++ Vector("# The turn", "rendered")
          )
        )
      }
    }

    test("a trial keeps its coder's diff from the task's base, whole") {
      withTask { f =>
        // out/daemon: the fake coder's own leftover, an empty file the diff names too.
        trial(f, Fake("object A { val y = 2 }")).map(_.diff.linesIterator.toVector) ==> Right(
          Vector(
            "diff --git a/a.scala b/a.scala",
            "index 69c493d..89f0635 100644",
            "--- a/a.scala",
            "+++ b/a.scala",
            "@@ -1 +1 @@",
            "-object A",
            "+object A { val y = 2 }",
            "diff --git a/out/daemon b/out/daemon",
            "new file mode 100644",
            "index 0000000..e69de29"
          )
        )
      }
    }

    test("a grit trial records its standing text by digest, and a Claude Code trial none") {
      withTask { f =>
        (
          gritTrial(f, GritFake(gritLines)).map(_.trial.standing.map(_.hex)),
          trial(f, Fake("object A { val x = 1 }")).map(_.trial.standing)
        ) ==> (
          // The sha256 of "Make the change.", the fixture's standing text, as `sha256sum` prints it.
          Right(Some("a0d3e561a1dc63765375a61b17f86f53d1650aee7c659e88fb79456e419a1365")),
          Right(None)
        )
      }
    }

    test(
      "grit runs `grit work` on its brief, in an environment of the rig's alone, its key and Jev's read from their files"
    ) {
      withTask { f =>
        gritTrial(f, GritFake(gritLines)).map { _ =>
          val argv = f.recorded("gargv").linesIterator.toVector
          (
            argv.headOption,
            argv.lift(1).exists(b => b.endsWith("/brief.md") && !b.startsWith(f.root.toString)),
            f.recorded("gbrief"),
            f.recorded("genv")
              .linesIterator
              .filterNot(l => Seq("PWD=", "OLDPWD=", "SHLVL=", "_=").exists(l.startsWith))
              .toVector
          )
        } ==> Right(
          (
            Some("work"),
            true,
            "Make the change.\n\n" + Rig.GritPreamble + "Give A an x.",
            Vector(
              "ANTHROPIC_API_KEY=sk-ant-test-0123",
              "GRIT_ASSEMBLER=linear",
              "GRIT_DATABASE_URL=jdbc:postgresql://localhost:5432/grit_bench_coder",
              "GRIT_DREAM_MODEL=anthropic/claude-haiku-5-5",
              "GRIT_DREAM_PROVIDER=api.anthropic.com",
              "GRIT_ENV_FILE=/dev/null",
              "GRIT_LIGHT_MODEL=anthropic/claude-haiku-5-5",
              "GRIT_LIGHT_PROVIDER=api.anthropic.com",
              "GRIT_MAX_TOKENS=8192",
              "GRIT_MODEL=anthropic/claude-haiku-5-5",
              "GRIT_PROVIDER=api.anthropic.com",
              "GRIT_SCOUT=/usr/bin/true",
              "GRIT_SESSION=bench",
              "GRIT_STRONG_MODEL=anthropic/claude-haiku-5-5",
              "GRIT_STRONG_PROVIDER=api.anthropic.com",
              "GRIT_SWEEP=24h",
              "GRIT_TOOLS=allowlist",
              "GRIT_TOOL_ROUNDS=4",
              s"HOME=${f.root}",
              s"JAVA_HOME=${sys.props("java.home")}",
              "JDK_JAVA_OPTIONS=-XX:MaxRAMPercentage=15",
              "JEV_API_KEY=jev-test-4567",
              "LANG=C.UTF-8",
              s"PATH=${sys.env.getOrElse("PATH", "/usr/bin:/bin")}"
            )
          )
        )
      }
    }

    test(
      "a grit trial with no JEV_API_KEY, or a blank one, is refused, saying its guard would have no classifier"
    ) {
      val refusal =
        "grit's machine guard would have no classifier, and would refuse every command it asks about"
      (
        Rig.guardKey(Map.empty),
        Rig.guardKey(Map("JEV_API_KEY" -> "  ")),
        Rig.guardKey(env).map(_.variable)
      ) ==> (
        Left(s"JEV_API_KEY is not set: $refusal"),
        Left(s"JEV_API_KEY is set but blank: $refusal"),
        Right(Variable("JEV_API_KEY"))
      )
    }

    test(
      "a grit coder whose call reported no cost is stopped at it, its trial kept not passed and its cost unknown, never zero"
    ) {
      withTask { f =>
        val unpriced = gritLines.take(2) :+
          ("[provider] deepseek/deepseek-v4.1-flash-20260910 @ fireworks answered · in 10 · " +
            "read 0 · write 0 · out 20 · cost unknown")
        gritTrial(f, GritFake(unpriced :+ "[provider] never read", hangs = true), deepseek).map {
          t =>
            (
              t.trial.passed,
              t.trial.usage.costUsd,
              t.stopped,
              t.lines.filter(_.startsWith("cost"))
            )
        } ==> Right(
          (
            false,
            None,
            Some(
              "a call of deepseek/deepseek-v4.1-flash-20260910 reported no cost, so its cap could not hold"
            ),
            Vector(
              "cost: unknown, 1 of 2 calls priced neither by grit's rates nor by their provider"
            )
          )
        )
      }
    }

    test(
      "a grit trial's report says how its cost was priced"
    ) {
      def at(model: String) =
        s"[provider] $model answered · in 10 · read 0 · write 0 · out 20 · "
      val reported = Vector(
        at("deepseek/deepseek-v4.1-flash-20260910 @ fireworks") + "0.29¢",
        at("deepseek/deepseek-v4.1-flash-20260910 @ fireworks") + "0.0035¢"
      )
      val mixed = gritLines.take(2) ++ reported.take(1)
      def printed(lines: Vector[String], caps: Caps) = withTask { f =>
        gritTrial(f, GritFake(lines), caps).map(
          _.lines
            .filter(l => l.startsWith("cost") || l.startsWith("trial"))
            .map(_.replaceAll("^trial: \\S+", "trial:"))
        )
      }
      (printed(gritLines, cap), printed(reported, deepseek), printed(mixed, deepseek)) ==> (
        Right(
          Vector(
            "trial: of fixture by anthropic/claude-haiku-5-5 @ api.anthropic.com, 3 s",
            "cost: 0.0089¢ by grit's rates"
          )
        ),
        Right(
          Vector(
            "trial: of fixture by deepseek/deepseek-v4.1-flash-20260910 @ fireworks, 3 s",
            "cost: 0.29¢ as the provider reported each call, printed to two significant digits"
          )
        ),
        Right(
          Vector(
            "trial: of fixture by deepseek/deepseek-v4.1-flash-20260910 @ fireworks, 3 s",
            "cost: 0.3¢, 1 of 2 calls as their provider reported them, printed to two significant digits, the rest by grit's rates"
          )
        )
      )
    }

    test("a grit coder whose calls reach the cap is stopped, and its trial kept not passed") {
      withTask { f =>
        val dear = gritLines.take(2) :+
          ("[provider] anthropic/claude-haiku-5-5 @ api.anthropic.com answered · in 10 · read 0 · " +
            "write 0 · out 200000 · 10¢")
        gritTrial(f, GritFake(dear :+ "[provider] never read", hangs = true)).map { t =>
          (t.trial.passed, t.trial.calls, t.stopped, t.lines.find(_.startsWith("ended:")))
        } ==> Right(
          (
            false,
            2,
            Some("its calls cost 10¢, reaching its cap of 5¢"),
            Some("ended: its calls cost 10¢, reaching its cap of 5¢, in error")
          )
        )
      }
    }

    test(
      "grit's answered lines reach the rig whole from its JVM, which starts with no locale of its own"
    ) {
      withTask { f =>
        gritTrial(f, GritFake(gritLines, jvm = true)).map(t =>
          (t.trial.calls, t.lines.filter(_.startsWith("cost")))
        ) ==> Right((2, Vector("cost: 0.0089¢ by grit's rates")))
      }
    }

    test("grit's output and the tail of its errors are kept beside the trial, refused or not") {
      withTask { f =>
        val refused = gritTrial(f, GritFake(Vector("[main] said")), turn = gritTurn)
        (
          refused.isLeft,
          Option.when(f.exists("grit.log"))(
            f.recorded("grit.log")
              .linesIterator
              .takeWhile(!_.startsWith("[rig] the coder's"))
              .toVector
          )
        ) ==> (
          true,
          Some(
            Vector(
              "[rig] compiled before the coder: ./mill --ticker false m.__.compile",
              "compiled",
              "[main] said",
              "[rig] the tail of grit's errors:",
              "a log line",
              "[rig] under: effort not recorded (set none); replay not recorded; window 100000 tokens; input limit 450000 tokens; last turn's end not recorded"
            )
          )
        )
      }
    }

    test("the checkout compiles what the task's checks name before grit starts, unbilled") {
      withTask { f =>
        gritTrial(f, GritFake(gritLines)).map(_ =>
          (
            Option.when(f.exists("mill-argv"))(f.recorded("mill-argv").linesIterator.toVector),
            f.recorded("grit.log").linesIterator.take(3).toVector
          )
        ) ==> Right(
          (
            Some(Vector("--ticker", "false", "m.__.compile")),
            Vector(
              "[rig] compiled before the coder: ./mill --ticker false m.__.compile",
              "compiled",
              gritLines.head
            )
          )
        )
      }
    }

    test(
      "an integration check's module is compiled before grit, never its testOnly task or its suite"
    ) {
      withTask { f =>
        val it = Check(Runner.It, Vector("n.it.testOnly", "n.engine.SomeTests"))
        val g = f.copy(task = f.task.copy(checks = f.task.checks :+ it))
        gritTrial(g, GritFake(gritLines)).map(_ =>
          Option.when(f.exists("mill-argv"))(f.recorded("mill-argv").linesIterator.toVector)
        ) ==> Right(Some(Vector("--ticker", "false", "m.__.compile", "+", "n.it.__.compile")))
      }
    }

    test(
      "a checkout whose base fetches shellcheck fetches it before the coder starts, and before a known run's checks"
    ) {
      withTask { f =>
        val coder = gritTrial(f, GritFake(gritLines)).map(_ => f.exists("gsaw-shellcheck"))
        Files.deleteIfExists(f.root.resolve("shellcheck-fetched"))
        val known =
          Rig.known(f.work)(f.origin, f.task, checkout).map(_ => f.exists("shellcheck-fetched"))
        (coder, known) ==> (Right(true), Right(true))
      }
    }

    test("a checkout that does not compile before grit is a refused trial, and grit never starts") {
      withTask { f =>
        Files.writeString(f.root.resolve("mill-fails"), "")
        (gritTrial(f, GritFake(gritLines)), f.exists("gargv")) ==> (
          Left("the checkout did not compile before the coder (exit 1): broken"),
          false
        )
      }
    }

    test(
      "a grit coder whose processes hold past the memory budget, or leave the machine under its floor, is stopped and kept not passed"
    ) {
      // A turn with no edit, as a run stopped before its first call has.
      val unedited = gritTurn.copy(messages = gritTurn.messages.take(1))
      def stopped(held: Memory.Held) = withTask { f =>
        gritTrial(f, GritFake(gritLines, hangs = true), turn = unedited, heldCoding = Right(held))
          .map(t => (t.trial.passed, t.stopped))
      }
      (stopped(Memory.Held(9L << 30, 16L << 30)), stopped(Memory.Held(1L << 30, 1L << 29))) ==> (
        Right((false, Some("its processes held 9 GB, past its memory budget of 8 GB"))),
        Right((false, Some("the machine had 0.5 GB available, under its floor of 1 GB")))
      )
    }

    test(
      "a pre-compile whose processes pass the memory budget is stopped, the trial refused naming it"
    ) {
      withTask { f =>
        Files.writeString(f.root.resolve("mill-heavy"), "")
        (gritTrial(f, GritFake(gritLines)), f.exists("gargv")) ==> (
          Left(
            "the checkout's compile before the coder was stopped: its processes held 9 GB, " +
              "past its memory budget of 8 GB"
          ),
          false
        )
      }
    }

    test(
      "a check whose processes pass the memory budget is stopped, the trial kept not passed naming it"
    ) {
      withTask { f =>
        Files.writeString(f.root.resolve("check-heavy"), "")
        gritTrial(f, GritFake(gritLines)).map(t =>
          (t.trial.passed, t.lines.filter(l => l.startsWith("check")))
        ) ==> Right(
          (
            false,
            Vector(
              "checks stopped: `scripts/check m` was stopped: its processes held 9 GB, past its " +
                "memory budget of 8 GB"
            )
          )
        )
      }
    }

    test("a grit coder whose memory cannot be read is refused before grit starts") {
      withTask { f =>
        (
          gritTrial(f, GritFake(gritLines), heldCoding = Left(HostError.Failed("no /proc here"))),
          f.exists("gargv")
        ) ==> (
          Left(
            "the memory grit's processes hold cannot be read, so its budget could not hold: " +
              "no /proc here"
          ),
          false
        )
      }
    }

    test(
      "a memory budget is a whole number of GB of at least 1, 24 GB by default, the floor always 1 GB"
    ) {
      (
        MemoryCap.of("12").map(c => (c.most, c.floor)),
        MemoryCap.of("0"),
        MemoryCap.of("1.5"),
        (MemoryCap.Default.most, MemoryCap.Default.floor)
      ) ==> (
        Right((12L << 30, 1L << 30)),
        Left("the memory budget 0 is not a whole number of GB of at least 1"),
        Left("the memory budget 1.5 is not a whole number of GB of at least 1"),
        (24L << 30, 1L << 30)
      )
    }

    test("the task's checks run with every JVM capped, as grit's own are") {
      withTask { f =>
        gritTrial(f, GritFake(gritLines)).map(_ =>
          Vector("check", "enola-law.sh").map(s =>
            Option.when(f.exists(s"$s-jvm"))(f.recorded(s"$s-jvm"))
          )
        ) ==> Right(Vector.fill(2)(Some("-XX:MaxRAMPercentage=15")))
      }
    }

    test("a grit trial the trial row refuses still prints its calls and its cost") {
      withTask { f =>
        val twice = gritTurn.copy(messages = gritTurn.messages.take(3) ++ gritTurn.messages.drop(1))
        gritTrial(f, GritFake(gritLines.take(2)), turn = twice) ==> Left(
          "call 1 call-1: input 1200 (cache read 1000, written 150)\n" +
            "cost: 0.0054¢ by grit's rates\n" +
            "thinking: not reported\n" +
            "a trial's calls before its first edit are 0 to its calls"
        )
      }
    }

    test(
      "a grit trial prints its thinking and its cost by grit's rates, or that a call did not report it"
    ) {
      // Haiku 5.5's short card: $0.50 a million output tokens; 30 + 6 thinking is 0.0018¢.
      val thought = gritLines.map(
        _.replace("out 40 ·", "out 40 (thinking 30) ·").replace("out 10 ·", "out 10 (thinking 6) ·")
      )
      val half = gritLines.map(_.replace("out 40 ·", "out 40 (thinking 30) ·"))
      def printed(lines: Vector[String]) = withTask { f =>
        gritTrial(f, GritFake(lines)).map(_.lines.filter(_.startsWith("thinking")))
      }
      (printed(thought), printed(half), printed(gritLines)) ==> (
        Right(Vector("thinking: 36 of 50 output tokens, 0.0018¢ by grit's rates")),
        Right(Vector("thinking: not reported by every call (30 over the 1 of 2 that did)")),
        Right(Vector("thinking: not reported"))
      )
    }

    test("a grit trial prints grit's cost alone, since grit estimates none of its own") {
      withTask { f =>
        gritTrial(f, GritFake(gritLines)).map(_.lines.filter(_.startsWith("cost"))) ==>
          Right(Vector("cost: 0.0089¢ by grit's rates"))
      }
    }

    test(
      "a grit trial's window and work turns reach grit's environment, the trial kept under grit"
    ) {
      withTask { f =>
        val settings = Arm.Settings(window = Some(Tokens(25_000)), workTurns = Some(3))
        gritTrial(f, GritFake(gritLines), settings = settings).map { t =>
          (
            Harness.value(t.trial.session.harness),
            f.recorded("genv")
              .linesIterator
              .filter(l => Seq("GRIT_WINDOW", "GRIT_WORK").exists(l.startsWith))
              .toVector
          )
        } ==> Right(("grit", Vector("GRIT_WINDOW_TOKENS=25000", "GRIT_WORK_TURNS=3")))
      }
    }

    test(
      "a grit trial's effort reaches grit as its turn's and its dreaming's effort, and a trial with none sets no effort"
    ) {
      withTask { f =>
        def efforts(settings: Arm.Settings) =
          gritTrial(f, GritFake(gritLines), settings = settings).map(_ =>
            f.recorded("genv").linesIterator.filter(_.contains("EFFORT")).toVector
          )
        (efforts(Arm.Settings(effort = Some(Effort.Medium))), efforts(Arm.Settings())) ==> (
          Right(Vector("GRIT_DREAM_EFFORT=medium", "GRIT_EFFORT=medium")),
          Right(Vector.empty)
        )
      }
    }

    test(
      "a grit trial's replay reaches grit in the catalog's word, and a trial with none sets none"
    ) {
      withTask { f =>
        def replays(settings: Arm.Settings) =
          gritTrial(f, GritFake(gritLines), settings = settings).map(_ =>
            f.recorded("genv").linesIterator.filter(_.contains("REPLAY")).toVector
          )
        (
          replays(Arm.Settings(replay = Some(ReasoningReplay.Latest))),
          replays(Arm.Settings(replay = Some(ReasoningReplay.Dropped))),
          replays(Arm.Settings())
        ) ==> (
          Right(Vector("GRIT_REPLAY=latest")),
          Right(Vector("GRIT_REPLAY=dropped")),
          Right(Vector.empty)
        )
      }
    }

    test(
      "a grit trial's report and its log say the grade and effort its turn's calls asked, the setting beside it when they differ, its window and input limit"
    ) {
      withTask { f =>
        def under(settings: Arm.Settings, ran: RanUnder) =
          gritTrial(
            f,
            GritFake(gritLines),
            turn = gritTurn.copy(ranUnder = ran),
            settings = settings
          )
            .map { t =>
              val log = Files.readString(f.root.resolve("grit.log")).linesIterator.toVector
              (t.lines.filter(_.startsWith("under:")), log.filter(_.contains("under:")))
            }
        def pin(grade: Grade, ref: ModelRef, effort: Option[Effort], replay: ReasoningReplay) =
          Pinned(
            CatalogVersion("v"),
            grade,
            Assignment(ref, 8192, effort),
            Settings.of(None).copy(replay = replay)
          )
        val (fireworks, anthropic) = (deepseek.pair, cap.pair)
        // Routed to its strong rung, which a hard root's allotment asks at high.
        val strong = RanUnder(
          Right(Some(pin(Grade.Strong, fireworks, Some(Effort.High), ReasoningReplay.Details))),
          Right(Some(Tokens(450_000))),
          Right(None)
        )
        val defaulted = RanUnder(
          Right(Some(pin(Grade.Standard, anthropic, None, ReasoningReplay.Latest))),
          Right(None),
          Right(None)
        )
        val high = "under: grade strong, effort high (set medium); replay all (set latest); " +
          "window 100000 tokens; input limit 450000 tokens; last turn's end not recorded"
        val medium = "under: grade standard, effort medium (grit's default); replay latest; " +
          "window 25000 tokens; input limit not recorded; last turn's end not recorded"
        val unread = "under: effort not read (set medium); replay not read (set none); window " +
          "100000 tokens; input limit not read; last turn's end not read"
        (
          under(
            Arm.Settings(effort = Some(Effort.Medium), replay = Some(ReasoningReplay.Latest)),
            strong
          ),
          under(
            Arm.Settings(window = Some(Tokens(25_000)), replay = Some(ReasoningReplay.Latest)),
            defaulted
          ),
          under(
            Arm.Settings(effort = Some(Effort.Medium), replay = Some(ReasoningReplay.Dropped)),
            RanUnder(Left("gone"), Left("gone"), Left("gone"))
          )
        ) ==> (
          Right((Vector(high), Vector(s"[rig] $high"))),
          Right((Vector(medium), Vector(s"[rig] $medium"))),
          Right((Vector(unread), Vector(s"[rig] $unread")))
        )
      }
    }

    test(
      "the grit arm's pin for Claude Haiku 5.5 at Anthropic's API is the catalog's for that pair, priced by its profile, asking no effort and so sent grit's default"
    ) {
      val (pin, catalog) = armPin(cap)
      (
        pin,
        pin.settings.pricing.isDefined,
        pin.assignment.effort.getOrElse(AnthropicProvider.DefaultEffort)
      ) ==> (
        catalog.pinned(Ladder.one(Assignment(cap.pair, 8192, None))).first,
        true,
        Effort.Medium
      )
      cap.pair.upstream ==> Some(Upstream.AnthropicApi)
    }

    test(
      "the grit arm's pin for DeepSeek v4.1 flash at fireworks is the catalog's for that pair, replaying details and sending an empty reasoning_content"
    ) {
      val (pin, catalog) = armPin(deepseek)
      (pin, pin.settings.replay, pin.settings.quirks) ==> (
        catalog.pinned(Ladder.one(Assignment(deepseek.pair, 8192, None))).first,
        ReasoningReplay.Details,
        Set(WireQuirk.EmptyReasoningContent)
      )
    }

    test(
      "a grit trial's under: line ends with what ended its last turn: its call budget, its input limit, its reply or its output budget, or that it recorded none or did not read"
    ) {
      def ended(by: Either[String, Option[TurnRecord.EndedBy]]) =
        Tried.under(Arm.Settings(), RanUnder(Right(None), Right(None), by)).split("; ").last
      Vector(
        Right(Some(TurnRecord.EndedBy.Budget)),
        Right(Some(TurnRecord.EndedBy.InputLimit)),
        Right(Some(TurnRecord.EndedBy.Reply)),
        Right(Some(TurnRecord.EndedBy.OutOfOutput)),
        Right(None),
        Left("gone")
      ).map(ended) ==> Vector(
        "last turn ended by its call budget",
        "last turn ended by its input limit",
        "last turn ended by its reply",
        "last turn ended by its output budget, all thinking and no reply",
        "last turn's end not recorded",
        "last turn's end not read"
      )
    }

    test(
      "caps name a Claude model at the Anthropic API, or an OpenRouter model at the upstream given; anything else is refused naming both forms"
    ) {
      def pair(model: String, upstream: Option[String]) =
        Caps.of(model, upstream, "0.05", "4").map(c => (c.pair.toString, c.claudeModel, c.key))
      (
        pair("claude-haiku-5-5", None),
        pair("deepseek/deepseek-v4.1-flash-20260910", Some("fireworks")),
        pair("deepseek/deepseek-v4.1-flash-20260910", None),
        pair("Deepseek flash", Some("fireworks")),
        pair("deepseek/deepseek-v4.1-flash-20260910", Some("Fire Works")),
        pair("deepseek/deepseek-v4.1-flash-20260910", Some("api.anthropic.com"))
      ) ==> (
        Right(
          (
            "anthropic/claude-haiku-5-5 @ api.anthropic.com",
            Some("claude-haiku-5-5"),
            Variable("ANTHROPIC_API_KEY")
          )
        ),
        Right(
          (
            "deepseek/deepseek-v4.1-flash-20260910 @ fireworks",
            None,
            Variable("OPENROUTER_API_KEY")
          )
        ),
        Left(
          "the model deepseek/deepseek-v4.1-flash-20260910 is not a Claude model's id, as claude-haiku-5-5, " +
            "and names no upstream, as an OpenRouter model's does (deepseek/deepseek-v4.1-flash-20260910 at fireworks)"
        ),
        Left(
          "the model Deepseek flash is not an OpenRouter model's id, as deepseek/deepseek-v4.1-flash-20260910"
        ),
        Left("the upstream Fire Works is not an OpenRouter upstream's slug, as fireworks"),
        Left(
          "the upstream api.anthropic.com is the Anthropic API, named by giving a Claude model's id, " +
            "as claude-haiku-5-5, and no upstream"
        )
      )
    }
    test("a budget is a decimal of USD above 0; anything else is refused naming it") {
      List("0.05", "0", "-0.01", "five", "Infinity").map(b =>
        Caps.of("claude-haiku-5-5", None, b, "4").map(_.budget.usd)
      ) ==> List(
        Right(BigDecimal("0.05")),
        Left("the budget 0 is not an amount of USD above 0"),
        Left("the budget -0.01 is not an amount of USD above 0"),
        Left("the budget five is not an amount of USD above 0"),
        Left("the budget Infinity is not an amount of USD above 0")
      )
    }

    test(
      "an OpenRouter grit coder runs its pair's upstream in every role it sets, handed its key as OPENROUTER_API_KEY"
    ) {
      withTask { f =>
        gritTrial(f, GritFake(gritLines), deepseek).map { _ =>
          f.recorded("genv")
            .linesIterator
            .filter(l =>
              l.contains("_API_KEY=") || l.contains("_PROVIDER=") || l.contains("MODEL=")
            )
            .toVector
        } ==> Right(
          Vector(
            "GRIT_DREAM_MODEL=deepseek/deepseek-v4.1-flash-20260910",
            "GRIT_DREAM_PROVIDER=fireworks",
            "GRIT_LIGHT_MODEL=deepseek/deepseek-v4.1-flash-20260910",
            "GRIT_LIGHT_PROVIDER=fireworks",
            "GRIT_MODEL=deepseek/deepseek-v4.1-flash-20260910",
            "GRIT_PROVIDER=fireworks",
            "GRIT_STRONG_MODEL=deepseek/deepseek-v4.1-flash-20260910",
            "GRIT_STRONG_PROVIDER=fireworks",
            "JEV_API_KEY=jev-test-4567",
            "OPENROUTER_API_KEY=sk-ant-test-0123"
          )
        )
      }
    }

    test("Claude Code is refused a pair not at the Anthropic API, and never starts") {
      withTask { f =>
        (trial(f, Fake("object A { val x = 1 }"), caps = deepseek), f.exists("argv")) ==> (
          Left(
            "Claude Code runs a Claude model at the Anthropic API, not deepseek/deepseek-v4.1-flash-20260910 @ fireworks"
          ),
          false
        )
      }
    }

    test(
      "grit is refused fewer than 2 rounds, a model with no rates, and the operator's database"
    ) {
      withTask { f =>
        val oneRound = Caps.of("claude-haiku-5-5", None, "0.05", "1").getOrElse(cap)
        val unpriced = Caps.of("claude-unknown-9", None, "0.05", "4").getOrElse(cap)
        val passed = Variables.of(Map.empty).getOrElse(throw new java.lang.AssertionError())
        def database(url: String) =
          Standing
            .of("c")
            .flatMap(Arm.Grit.of("/bin/grit", "/usr/bin/true", _, url, 8192, passed))
            .map(_ => url)
        (
          gritTrial(f, GritFake(gritLines), oneRound),
          gritTrial(f, GritFake(gritLines), unpriced),
          database("jdbc:postgresql://localhost:5432/grit"),
          database("jdbc:postgresql://localhost:5432/"),
          f.exists("gargv")
        ) ==> (
          Left("grit takes at least 2 rounds, its first call and one after a tool's; not 1"),
          Left("grit's rates price no model claude-unknown-9, so its cap could not stop it"),
          Left("the grit coder's database grit is the operator's own; name one of the bench's"),
          Left("the grit coder's database URL names no database"),
          false
        )
      }
    }

    test(
      "a dry grit run prints what its first call would send, handed no key, and leaves nothing"
    ) {
      withTask { f =>
        val arm = f.gritArm(GritFake(Vector("# The first call", "system: …")))
        (
          Rig.dry(f.work)(f.origin, f.task, arm, cap, checkout),
          f.recorded("gargv").linesIterator.toVector.take(2),
          f.recorded("genv").linesIterator.exists(_.startsWith("ANTHROPIC_API_KEY")),
          f.exists("trial")
        ) ==> (Right("# The first call\nsystem: …"), Vector("work", "--dry"), false, false)
      }
    }

    test(
      "a new run clears the work root of every run's checkout, and of logs past the newest many"
    ) {
      val root = Files.createTempDirectory("grit-runs")
      try {
        val runs = Vector("a-100", "a-101", "b-102", "b-103")
        runs.foreach { n =>
          Files.createDirectories(root.resolve(n).resolve("src"))
          Files.writeString(root.resolve(s"$n.log"), "x")
        }
        Files.createDirectories(root.resolve("a-99-known"))
        Files.createDirectories(root.resolve("notes"))
        (
          Rig.cleared(root)(runs = 3),
          Files.list(root).iterator.asScala.map(_.getFileName.toString).toVector.sorted
        ) ==> (
          Right(6),
          Vector("a-101.log", "b-102.log", "b-103.log", "notes")
        )
      } finally Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
    }

    test("a known run passes when its commits apply and its checks pass, and leaves nothing") {
      withTask { f =>
        (Rig.known(f.work)(f.origin, f.task, checkout).map(_.passed), f.exists("trial")) ==>
          (Right(true), false)
      }
    }
  }
}
