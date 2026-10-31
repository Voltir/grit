package grit.eval.harness.bench

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.time.Instant

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import grit.core.clock.Clock
import grit.core.host.{Argv, Processes, RelPath}
import grit.core.message.{Tokens, Usage}
import grit.dbos.internal.BenchRows
import grit.host.checkout.LocalProcesses

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

  private val key = ApiKey.of("sk-ant-test-0123").getOrElse(throw new java.lang.AssertionError())
  private val cap =
    Caps.of("claude-haiku-5-5", "0.05", "4").getOrElse(throw new java.lang.AssertionError())
  private val checkout = rel("trial")

  /** The fake's run, its edit of `a.scala` writing `edited`, its stream's lines `lines`. */
  private final case class Fake(edited: String, lines: Vector[String] = Streams.run)

  private final case class Fixture(work: Processes, root: Path, origin: String, task: Task) {
    def arm(fake: Fake): Arm = {
      Files.write(root.resolve("stream.jsonl"), fake.lines.asJava)
      val claude = root.resolve("claude")
      Files.writeString(
        claude,
        s"""#!/bin/sh
           |printf '%s\\n' "$$@" > $root/argv
           |printf '%s' "$${ANTHROPIC_API_KEY-unset}" > $root/key
           |printf '%s\\n' '${fake.edited}' > a.scala
           |mkdir -p out && touch out/daemon
           |sed "s|${Streams.Checkout}|$$(pwd -P)|g" $root/stream.jsonl
           |exit 1
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
      val work = new LocalProcesses(root, sys.env)
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
      body(Fixture(work, root, origin.toString, task))
    } finally Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
  }

  private def trial(f: Fixture^, fake: Fake): Either[String, Tried] =
    Rig.trial(f.work, stepping())(f.origin, f.task, f.arm(fake), cap, key, checkout)

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
            Usage(Tokens(6262), Tokens(90), Tokens(4100), None, Tokens(2150)),
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

    test("the key reaches claude through its environment alone, never its arguments") {
      withTask { f =>
        val kept = trial(f, Fake("object A { val x = 1 }"))
        (
          f.recorded("key"),
          f.recorded("argv").contains("sk-ant"),
          kept.map(t => (t.toString + t.transcript.mkString).contains("sk-ant"))
        ) ==> ("sk-ant-test-0123", false, Right(false))
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

    test("a trial removes its checkout; a run that fails keeps it, less its build output") {
      withTask { f =>
        val kept = trial(f, Fake("object A { val x = 1 }")).map(_ => f.exists("trial"))
        val failed = trial(f, Fake("object A { val x = 1 }", Vector.empty))
        (kept, failed.isLeft, f.exists("trial/a.scala"), f.exists("trial/out")) ==>
          (Right(false), true, true, false)
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
