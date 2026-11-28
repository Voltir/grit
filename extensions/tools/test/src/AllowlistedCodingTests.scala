package grit.tools

import scala.concurrent.duration.*

import grit.core.flow.TestTainted
import grit.core.host.*
import grit.core.host.AllowlistTests.Noting
import grit.core.id.{TestCallSlots, ToolCallId}
import grit.core.message.AssistantBlock
import grit.core.place.{Directory, Place}
import grit.core.session.Event
import grit.core.tool.{Believed, Bound, Outcome, Repairs, ToolName, Toolbox}
import grit.core.visibility.Integrity

import utest.*

/** [[Coding.allowlisted]]'s tools over a scripted checkout and noting runs: what asks, what
  * reaches the runs, and what the model is told and reads back.
  */
object AllowlistedCodingTests extends TestSuite {

  private def argv(words: String*): Argv =
    Argv.of(words.head, words.tail*).fold(e => throw new java.lang.AssertionError(e), identity)

  private def allowlist(longest: FiniteDuration): Allowlist =
    Allowlist
      .of(
        Vector(
          Allowlist.Rule.Exactly(argv("git", "status")),
          Allowlist.Rule.Leading(argv("git", "commit")),
          Allowlist.Rule.Leading(argv("./mill")),
          Allowlist.Rule.Following(argv("scripts/check"), "the suites or modules to check")
        ),
        longest
      )
      .fold(e => throw new java.lang.AssertionError(e), identity)

  /** [[allowlist]]'s rules with its two builds, `./mill` and `scripts/check`, cleared. */
  private val building: Allowlist = {
    val mill = Allowlist.Rule.Leading(argv("./mill"))
    val check = Allowlist.Rule.Following(argv("scripts/check"), "the suites or modules to check")
    Allowlist
      .of(
        Vector(Allowlist.Rule.Exactly(argv("git", "status")), mill, check),
        2.minutes,
        Vector(mill, check)
      )
      .fold(e => throw new java.lang.AssertionError(e), identity)
  }

  /** git's status, restore, checkout of files and stash, `grep`, and `scripts/check`: forms a
    * refusal may name.
    */
  private val refusing: Allowlist =
    Allowlist
      .of(
        Vector(
          Allowlist.Rule.Leading(argv("git", "status")),
          Allowlist.Rule.Leading(argv("git", "restore")),
          Allowlist.Rule.Following(argv("git", "checkout", "--"), "the files to restore"),
          Allowlist.Rule.Leading(argv("git", "stash")),
          Allowlist.Rule.Leading(argv("grep")),
          Allowlist.Rule.Following(argv("scripts/check"), "the suites or modules to check")
        ),
        2.minutes
      )
      .fold(e => throw new java.lang.AssertionError(e), identity)

  /** What `rm -rf 'a b'` is refused with under [[allowlist]]. */
  private val rmRefused: Outcome =
    Outcome.Failed(
      s"${HostError.Unlisted(argv("rm", "-rf", "a b")).message} Only these programs run here: " +
        "`git`, `./mill`, `scripts/check`, each as the run tool's description lists it."
    )

  /** What the scripted commands do: `git status` ends at once, `./mill compile` after two
    * minutes, and `./mill hang` and `./mill quiet`, which writes nothing, never.
    */
  private val scripts = Map(
    argv("git", "status") -> ScriptedRuns.Script("ok\n", "", Some(Duration.Zero), 0),
    argv("./mill", "compile") -> ScriptedRuns.Script("compiling\n", "done\n", Some(2.minutes), 0),
    argv("./mill", "hang") -> ScriptedRuns.Script("started\n", "", None, 0),
    argv("./mill", "quiet") -> ScriptedRuns.Script("", "", None, 0),
    argv("scripts/check", "grit.tools") -> ScriptedRuns
      .Script("1 passed\n", "", Some(Duration.Zero), 0),
    argv("./mill", "test") -> ScriptedRuns.Script(long, "1 FAILED\n", Some(2.minutes), 1)
  )

  /** Line `n` of the long output `./mill test` writes at once. */
  private def step(n: Int): String = n match {
    case 100 => "FAIL grit.pool.PoolTests > a lease returned is reused"
    case 101 => "  assertion failed: ==> assertion failed: 1 != 2"
    case _ => f"step $n%03d went as it should"
  }

  /** 400 lines, 11 KB: past what an answer shows whole. */
  private lazy val long = (1 to 400).map(step).mkString("", "\n", "\n")

  /** The id the scripted runs give their `n`th run. */
  private def nth(n: Int): String = RunId.of(s"scripted-$n").written

  private def tools(
      host: CodingTests.Scripted^,
      runs: Allowlisted^,
      guard: MachineGuard^ = GuardFixtures.harmless,
      refusals: Refusals^ = Refusals.kept(),
      sources: Option[SourceTools] = None,
      tree: Uncommitted^ = Leaving.Nothing,
      compile: Option[Argv] = None
  ): Toolbox[caps.CapSet^{host, runs, guard, refusals, tree}] =
    Coding
      .allowlisted(host, host, runs, guard, refusals, sources, tree, compile)
      .fold(d => throw new java.lang.AssertionError(d), identity)

  /** A checkout whose every read of what a commit would leave out answers `files`, counted. */
  private final class Leaving(files: Either[HostError, Uncommitted.Files]) extends Uncommitted {
    val reads = new java.util.concurrent.atomic.AtomicInteger
    def read: Either[HostError, Uncommitted.Files] = { val _ = reads.incrementAndGet(); files }
  }

  private object Leaving {
    def Nothing: Leaving^ = new Leaving(Right(Uncommitted.Files(Vector.empty, Vector.empty)))
    def of(unstaged: Vector[String], untracked: Vector[String]): Leaving^ = {
      def paths(ps: Vector[String]) = ps.flatMap(RelPath.of(_).toOption)
      new Leaving(Right(Uncommitted.Files(paths(unstaged), paths(untracked))))
    }
  }

  private val instant = ScriptedRuns.Script("", "", Some(Duration.Zero), 0)

  /** `git args` run against the allowlisted tools with `tree` as the checkout's files. */
  private def commit(runs: Allowlisted^, tree: Uncommitted^, args: String*) =
    compiling(runs, tree, None, args*)

  /** As [[commit]], with `compile` declared. */
  private def compiling(
      runs: Allowlisted^,
      tree: Uncommitted^,
      compile: Option[Argv],
      args: String*
  ) =
    settle(
      new CodingTests.Scripted(),
      runs,
      "run",
      ujson.Obj("program" -> "git", "args" -> ujson.Arr.from(args)),
      tree = tree,
      compile = compile
    )

  /** The compile a repository declares in the commit tests: `./mill __.compile`. */
  private val build = argv("./mill", "__.compile")
  private val committing = argv("git", "commit", "-m", "x")

  private def call(name: String, args: ujson.Value): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(ToolCallId("c1"), name, args)

  /** `name` called with `args` against the allowlisted tools, as a request at [[worktree]] reaches
    * them: whether it asks, and what it came to when run.
    */
  private def settle(
      host: CodingTests.Scripted,
      runs: Allowlisted^,
      name: String,
      args: ujson.Value,
      guard: MachineGuard^ = GuardFixtures.harmless,
      refusals: Refusals^ = Refusals.kept(),
      at: grit.core.id.CallSlot = TestCallSlots.First,
      sources: Option[SourceTools] = None,
      tree: Uncommitted^ = Leaving.Nothing,
      compile: Option[Argv] = None
  ): Either[String, (Boolean, Outcome)] =
    tools(host, runs, guard, refusals, sources, tree, compile)
      .requested(
        call(name, args),
        Repairs.All,
        Option.when(CodingTests.Changing(name))(worktree),
        worktree
      )
      .left
      .map(_.message)
      .map {
        case free: Bound.Free => (false, free(at))
        case _: Bound.Gated => (true, Outcome.Failed("asks first"))
        case _ => (false, Outcome.Failed("not run here"))
      }

  /** The described tool `name` offers in the allowlisted set. */
  private def described(name: String): Vector[String] =
    Coding
      .allowlistedHosted(allowlist(2.minutes))
      .map(_.entry)
      .collect { case e if ToolName.value(e.name) == name => e.does }

  private val worktree =
    Directory
      .of("/work/grit")
      .map(Place.of)
      .fold(e => throw new java.lang.AssertionError(e), identity)

  /** What the engine's `name`, called with `args`, logs of the answer `shown`. */
  private def logs(name: String, args: ujson.Value, shown: String) =
    Toolbox
      .of(Coding.allowlistedHosted(allowlist(2.minutes))*)
      .fold(d => throw new java.lang.AssertionError(d), identity)
      .bind(TestTainted.authority(call(name, args)), Repairs.All)
      .map {
        case hosted: Bound.Hosted => hosted.logs(worktree, shown)
        case _ => None
      }

  val tests = Tests {
    test("the engine offers the tools an edge runs: the same names, schemas, gates and retries") {
      val list = allowlist(2.minutes)
      Coding.allowlistedHosted(list).map(_.entry) ==>
        tools(new CodingTests.Scripted(), Allowlisted.over(new Noting(scripts), list)).set.tools
    }

    test(
      "none asks first; write, edit, run and stop require member; every result is believed at member"
    ) {
      val list = allowlist(2.minutes)
      tools(new CodingTests.Scripted(), Allowlisted.over(new Noting(scripts), list)).set.tools
        .map(e => (ToolName.value(e.name), e.asks, e.requires)) ==> Vector(
        ("read", false, Integrity.Untrusted),
        ("list", false, Integrity.Untrusted),
        ("search", false, Integrity.Untrusted),
        ("write", false, Integrity.Member),
        ("edit", false, Integrity.Member),
        ("run", false, Integrity.Member),
        ("tail", false, Integrity.Untrusted),
        ("stop", false, Integrity.Member)
      )
      Coding.allowlistedHosted(list).map(_.output).distinct ==> Vector(
        Believed.At(Integrity.Member)
      )
    }

    test(
      "a listed run runs unasked; an unlisted one fails in Unlisted's words, running nothing"
    ) {
      val noted = new Noting(scripts)
      val runs = Allowlisted.over(noted, allowlist(2.minutes))
      val host = new CodingTests.Scripted()
      (
        settle(host, runs, "run", ujson.Obj("program" -> "git", "args" -> ujson.Arr("status"))),
        settle(host, runs, "run", ujson.Obj("program" -> "rm", "args" -> ujson.Arr("-rf", "a b")))
      ) ==> (
        Right((false, Outcome.Done("Exit code 0. Output:\nok\n"))),
        Right((false, rmRefused))
      )
      noted.log ==> Vector((argv("git", "status"), 2.minutes))
    }

    test(
      "a listed run the machine guard refuses fails in its words, starting nothing; an unlisted one is refused by the list, the guard not asked"
    ) {
      val noted = new Noting(scripts)
      val runs = Allowlisted.over(noted, allowlist(2.minutes))
      val host = new CodingTests.Scripted()
      val jev = new GuardFixtures.Judging(Right(Vector(0.0, 0.0, 0.0, 0.9)))
      val guard = GuardFixtures.guard(Right(jev))
      def ran(program: String, args: String*) =
        settle(host, runs, "run", ujson.Obj("program" -> program, "args" -> args), guard)
      val refused =
        "Nothing ran: the check that guards this machine judged that this command would send " +
          "or change something on another machine. Commands here may change only what is inside the " +
          "checkout /home/coder/work/co: rewrite it to stay inside, or leave that step to a person."
      (
        ran("./mill", "clean"),
        ran("rm", "-rf", "a b"),
        ran("git", "status"),
        noted.log,
        jev.states.size
      ) ==> (
        Right((false, Outcome.Failed(refused))),
        Right((false, rmRefused)),
        Right((false, Outcome.Done("Exit code 0. Output:\nok\n"))),
        Vector((argv("git", "status"), 2.minutes)),
        1
      )
    }

    test("write and edit run unasked") {
      val host = new CodingTests.Scripted()
      val runs = Allowlisted.over(new Noting(scripts), allowlist(2.minutes))
      (
        settle(host, runs, "write", ujson.Obj("path" -> "a.txt", "content" -> "x")),
        settle(
          host,
          runs,
          "edit",
          ujson.Obj(
            "path" -> "a.txt",
            "edits" -> ujson.Arr(ujson.Obj("oldText" -> "x", "newText" -> "y"))
          )
        )
      ) ==> (
        Right((false, Outcome.Done("Wrote a.txt: 1 line."))),
        Right((false, Outcome.Done("Edited a.txt: 1 replacement, starting on line 3.")))
      )
      host.log ==> Vector("write a.txt 1", "edit a.txt Replace(x,y)")
    }

    test("run's timeout is at most the allowlist's longest, and that when not given") {
      val noted = new Noting(scripts)
      val short = Allowlisted.over(noted, allowlist(30.seconds))
      val host = new CodingTests.Scripted()
      val status = ujson.Obj("program" -> "git", "args" -> ujson.Arr("status"))
      (
        settle(host, short, "run", status).map(_._2),
        settle(
          host,
          short,
          "run",
          ujson.Obj.from(status.value ++ Seq("timeout" -> ujson.Num(31)))
        ).isLeft,
        settle(
          host,
          Allowlisted.over(noted, allowlist(2.minutes)),
          "run",
          ujson.Obj.from(status.value ++ Seq("timeout" -> ujson.Num(90)))
        ).map(_._2)
      ) ==> (
        Right(Outcome.Done("Exit code 0. Output:\nok\n")),
        true,
        Right(Outcome.Done("Exit code 0. Output:\nok\n"))
      )
      noted.log.map(_._2) ==> Vector(30.seconds, 90.seconds)
    }

    test("a run still going after its wait answers its id and output so far; tail sees it end") {
      val runs = Allowlisted.over(new Noting(scripts), allowlist(10.minutes))
      val host = new CodingTests.Scripted()
      val id = nth(1)
      (
        settle(host, runs, "run", ujson.Obj("program" -> "./mill", "args" -> ujson.Arr("compile"))),
        settle(host, runs, "tail", ujson.Obj("id" -> id)),
        settle(host, runs, "tail", ujson.Obj("id" -> id, "wait" -> 600))
      ) ==> (
        Right(
          (
            false,
            Outcome.Done(
              s"Still running after 30 seconds, as `$id`; it is killed at 600 seconds. Its " +
                s"output so far:\ncompiling\n\nCall `tail` with id `$id` to see how it stands, " +
                "giving `wait` to wait for it to end, or `stop` to end it."
            )
          )
        ),
        Right(
          (
            false,
            Outcome.Done(
              s"`./mill compile`\nStill running after 30 seconds, as `$id`; it is killed at 600 " +
                s"seconds. Its output so far:\ncompiling\n\nCall `tail` with id `$id` to see how " +
                "it stands, giving `wait` to wait for it to end, or `stop` to end it."
            )
          )
        ),
        Right((false, Outcome.Done("`./mill compile`\nExit code 0. Output:\ncompiling\ndone\n")))
      )
    }

    test("run's wait, when given, is how long it waits before answering with the id") {
      val runs = Allowlisted.over(new Noting(scripts), allowlist(10.minutes))
      val host = new CodingTests.Scripted()
      val compile = ujson.Obj("program" -> "./mill", "args" -> ujson.Arr("compile"))
      (
        settle(host, runs, "run", ujson.Obj.from(compile.value ++ Seq("wait" -> ujson.Num(0)))),
        settle(host, runs, "run", ujson.Obj.from(compile.value ++ Seq("wait" -> ujson.Num(600))))
      ) ==> (
        Right(
          (
            false,
            Outcome.Done(
              s"Still running after 0 seconds, as `${nth(1)}`; it is killed at 600 seconds. Its " +
                s"output so far:\ncompiling\n\nCall `tail` with id `${nth(1)}` to see how it " +
                "stands, giving `wait` to wait for it to end, or `stop` to end it."
            )
          )
        ),
        Right((false, Outcome.Done("Exit code 0. Output:\ncompiling\ndone\n")))
      )
    }

    test("run's wait past 600 seconds waits 600, and its answer says so") {
      val runs = Allowlisted.over(new Noting(scripts), allowlist(20.minutes))
      val host = new CodingTests.Scripted()
      val hang = ujson.Obj("program" -> "./mill", "args" -> ujson.Arr("hang"), "wait" -> 900)
      settle(host, runs, "run", hang) ==> Right(
        (
          false,
          Outcome.Done(
            s"Still running after 600 seconds, as `${nth(1)}`; it is killed at 1200 seconds. " +
              s"Its output so far:\nstarted\n\nCall `tail` with id `${nth(1)}` to see how it " +
              "stands, giving `wait` to wait for it to end, or `stop` to end it.\n\n" +
              "`wait` is at most 600 seconds, so this waited up to 600, not 900."
          )
        )
      )
    }

    test("tail's wait past 600 seconds waits 600, and its answer says so") {
      val runs = Allowlisted.over(new Noting(scripts), allowlist(20.minutes))
      val host = new CodingTests.Scripted()
      val hang = ujson.Obj("program" -> "./mill", "args" -> ujson.Arr("hang"), "wait" -> 0)
      assert(settle(host, runs, "run", hang).isRight)
      settle(host, runs, "tail", ujson.Obj("id" -> nth(1), "wait" -> 900)) ==> Right(
        (
          false,
          Outcome.Done(
            s"`./mill hang`\nStill running after 600 seconds, as `${nth(1)}`; it is killed at " +
              s"1200 seconds. Its output so far:\nstarted\n\nCall `tail` with id `${nth(1)}` to " +
              "see how it stands, giving `wait` to wait for it to end, or `stop` to end it.\n\n" +
              "`wait` is at most 600 seconds, so this waited up to 600, not 900."
          )
        )
      )
    }

    test(
      "a program sent with its arguments runs as its first word, and one repeated as its first argument runs once; the answer says how it was read"
    ) {
      val noting = new Noting(scripts)
      val runs = Allowlisted.over(noting, allowlist(2.minutes))
      val host = new CodingTests.Scripted()
      (
        settle(host, runs, "run", ujson.Obj("program" -> "git status")),
        settle(
          host,
          runs,
          "run",
          ujson.Obj("program" -> "git", "args" -> ujson.Arr("git", "status"))
        )
      ) ==> (
        Right(
          (
            false,
            Outcome.Done(
              "Exit code 0. Output:\nok\n\n`program` takes the program alone, so `git status` " +
                "ran as `program` `git` with `status` first among `args`."
            )
          )
        ),
        Right(
          (
            false,
            Outcome.Done(
              "Exit code 0. Output:\nok\n\n`args` holds what follows the program, so its first " +
                "`git`, which repeated `program`, was left out."
            )
          )
        )
      )
      noting.log.map(_._1) ==> Vector(argv("git", "status"), argv("git", "status"))
    }

    test(
      "a program named without its directory runs as the listed one of that name, a build sent as another build's first argument runs as that build, and a build's own name repeated first is left out; the answer says how it was read"
    ) {
      val noting = new Noting(scripts)
      val runs = Allowlisted.over(noting, building)
      val host = new CodingTests.Scripted()
      def ran(args: ujson.Obj) = settle(host, runs, "run", args)
      (
        ran(ujson.Obj("program" -> "check", "args" -> ujson.Arr("grit.tools"))),
        ran(ujson.Obj("program" -> "./mill", "args" -> ujson.Arr("scripts/check", "grit.tools"))),
        ran(ujson.Obj("program" -> "scripts/check", "args" -> ujson.Arr("check", "grit.tools")))
      ) ==> (
        Right(
          (
            false,
            Outcome.Done(
              "Exit code 0. Output:\n1 passed\n\n`check` is not on the list, so it ran as " +
                "`scripts/check`, the listed program of that name."
            )
          )
        ),
        Right(
          (
            false,
            Outcome.Done(
              "Exit code 0. Output:\n1 passed\n\n`scripts/check` is a program of its own, not " +
                "an argument of `./mill`, so it ran as `program` with the arguments after it."
            )
          )
        ),
        Right(
          (
            false,
            Outcome.Done(
              "Exit code 0. Output:\n1 passed\n\n`args` holds what follows the program, so " +
                "its first `check`, which repeated `program` `scripts/check`, was left out."
            )
          )
        )
      )
      noting.log.map(_._1) ==> Vector.fill(3)(argv("scripts/check", "grit.tools"))
    }

    test(
      "a build's first argument is read as a build only when the list clears both: otherwise `./mill scripts/check` runs as sent"
    ) {
      val noting = new Noting(scripts)
      val runs = Allowlisted.over(noting, allowlist(2.minutes))
      val host = new CodingTests.Scripted()
      val _ = settle(
        host,
        runs,
        "run",
        ujson.Obj("program" -> "./mill", "args" -> ujson.Arr("scripts/check", "grit.tools"))
      )
      noting.log.map(_._1) ==> Vector(argv("./mill", "scripts/check", "grit.tools"))
    }

    test(
      "a program sent with shell text, or a split or repeat the list does not permit either, is refused as sent"
    ) {
      val noting = new Noting(scripts)
      val runs = Allowlisted.over(noting, allowlist(2.minutes))
      val host = new CodingTests.Scripted()
      // What a refusal says the command was: the words before "is not on this checkout's…".
      def refused(sent: ujson.Obj): Option[String] =
        settle(host, runs, "run", sent).toOption.collect { case (_, Outcome.Failed(why)) =>
          why.indexOf(" is not on") match {
            case -1 => why
            case at => why.take(at)
          }
        }
      (
        refused(ujson.Obj("program" -> "./mill compile && rm -rf x")),
        refused(ujson.Obj("program" -> "git log")),
        refused(ujson.Obj("program" -> "git", "args" -> ujson.Arr("git", "log")))
      ) ==> (
        Some("`'./mill compile && rm -rf x'`"),
        Some("`'git log'`"),
        Some("`git git log`")
      )
      noting.log ==> Vector.empty
    }

    test(
      "a long output is answered as its size and file, its failing lines and its tail, going and ended"
    ) {
      val runs = Allowlisted.over(new Noting(scripts), allowlist(10.minutes))
      val host = new CodingTests.Scripted()
      val id = nth(1)
      def shown(total: Int, kb: Int, tail: Seq[String]): String =
        s"$total lines, $kb KB, too long to show whole. All of it is in `.grit/runs/$id.out`, " +
          s"for `read` and `search`.\n\nOf lines 1 to ${total - 40}, those that look like " +
          "errors, warnings or failing tests, each after its line number in that file, and the " +
          "3 after each:\n" +
          "   100\tFAIL grit.pool.PoolTests > a lease returned is reused\n" +
          "   101\t  assertion failed: ==> assertion failed: 1 != 2\n" +
          "   102\tstep 102 went as it should\n   103\tstep 103 went as it should\n" +
          s"   104\tstep 104 went as it should\n\nIts last lines, ${total - 39} to $total:\n" +
          tail.mkString("\n")
      (
        settle(host, runs, "run", ujson.Obj("program" -> "./mill", "args" -> ujson.Arr("test"))),
        settle(host, runs, "tail", ujson.Obj("id" -> id, "wait" -> 600))
      ) ==> (
        Right(
          (
            false,
            Outcome.Done(
              s"Still running after 30 seconds, as `$id`; it is killed at 600 seconds. Its " +
                "output so far: " + shown(400, 11, (361 to 400).map(step)) +
                s"\n\nCall `tail` with id `$id` to see how it stands, giving `wait` to wait for " +
                "it to end, or `stop` to end it."
            )
          )
        ),
        Right(
          (
            false,
            Outcome.Done(
              "`./mill test`\nExit code 1. Output: " +
                shown(401, 11, (362 to 400).map(step) :+ "1 FAILED")
            )
          )
        )
      )
    }

    test(
      "a long failed run's answer, read back, is its flagged lines with the 3 after each, then those its tail flags, numbered as in its file"
    ) {
      val runs = Allowlisted.over(new Noting(scripts), allowlist(10.minutes))
      val host = new CodingTests.Scripted()
      val test = ujson.Obj("program" -> "./mill", "args" -> ujson.Arr("test"))
      val tail = ujson.Obj("id" -> nth(1), "wait" -> 600)
      def answer(name: String, args: ujson.Value): String =
        settle(host, runs, name, args) match {
          case Right((_, done: Outcome.Done)) => done.text
          case other => throw new java.lang.AssertionError(other.toString)
        }
      val ran =
        Vector(call("run", test) -> answer("run", test), call("tail", tail) -> answer("tail", tail))
      FailedRuns.last(FailedRunsTests.turn(ran*), None) ==> Some(
        grit.core.store.CheckoutState.Failed.of(
          "./mill test",
          Some(1),
          "   100\tFAIL grit.pool.PoolTests > a lease returned is reused\n" +
            "   101\t  assertion failed: ==> assertion failed: 1 != 2\n" +
            "   102\tstep 102 went as it should\n   103\tstep 103 went as it should\n" +
            "   104\tstep 104 went as it should\n…\n   401\t1 FAILED"
        )
      )
    }

    test("stop ends a run, answering Stopped with its output until then; again, the same") {
      val runs = Allowlisted.over(new Noting(scripts), allowlist(10.minutes))
      val host = new CodingTests.Scripted()
      val hang = ujson.Obj("program" -> "./mill", "args" -> ujson.Arr("hang"))
      val stopped =
        Outcome.Done("`./mill hang`\nStopped after 30 seconds. Its output until then:\nstarted\n")
      (
        settle(host, runs, "run", hang).isRight,
        settle(host, runs, "stop", ujson.Obj("id" -> nth(1))),
        settle(host, runs, "stop", ujson.Obj("id" -> nth(1)))
      ) ==> (true, Right((false, stopped)), Right((false, stopped)))
    }

    test("a run that has written nothing says so, going and stopped") {
      val runs = Allowlisted.over(new Noting(scripts), allowlist(10.minutes))
      val host = new CodingTests.Scripted()
      val quiet = ujson.Obj("program" -> "./mill", "args" -> ujson.Arr("quiet"))
      (
        settle(host, runs, "run", quiet).map(_._2),
        settle(host, runs, "stop", ujson.Obj("id" -> nth(1))).map(_._2)
      ) ==> (
        Right(
          Outcome.Done(
            s"Still running after 30 seconds, as `${nth(1)}`; it is killed at 600 seconds. No " +
              s"output yet.\n\nCall `tail` with id `${nth(1)}` to see how it stands, giving " +
              "`wait` to wait for it to end, or `stop` to end it."
          )
        ),
        Right(Outcome.Done("`./mill quiet`\nStopped after 30 seconds; no output."))
      )
    }

    test("tail or stop of an id never kept fails in Gone's words; a malformed id is refused") {
      val runs = Allowlisted.over(new Noting(scripts), allowlist(10.minutes))
      val host = new CodingTests.Scripted()
      val never = RunId.of("never")
      val gone = Outcome.Failed(HostError.Gone(never).message)
      (
        settle(host, runs, "tail", ujson.Obj("id" -> never.written)),
        settle(host, runs, "stop", ujson.Obj("id" -> never.written)),
        settle(host, runs, "tail", ujson.Obj("id" -> "xyz")).left.map(
          _.contains("`xyz` is not a run's id: ids are 8 letters and digits from 0-9 and a-f.")
        )
      ) ==> (Right((false, gone)), Right((false, gone)), Left(true))
    }

    test("Gone and Busy say what happened and what to do") {
      // What the model reads, word for word.
      (HostError.Gone(RunId.of("never")).message, HostError.Busy(4).message) ==> (
        s"No command `${RunId.of("never").written}` is kept in this checkout: grit restarted " +
          "since it started, or it ended and was forgotten after newer commands ended. Its " +
          "output is gone; run it again to see it.",
        "4 commands are already running in this checkout, so nothing started. Wait for one with " +
          "`tail`, or end one with `stop`."
      )
    }

    test(
      "an unlisted command's refusal names what works: the tool, one program with no shell, the listed form, or the programs that run"
    ) {
      val runs = Allowlisted.over(new Noting(scripts), refusing)
      val host = new CodingTests.Scripted()
      def refused(program: String, args: String*): Either[String, (Boolean, Outcome)] =
        settle(host, runs, "run", ujson.Obj("program" -> program, "args" -> args))
      def unlisted(written: String, hint: String) =
        Right(
          (
            false,
            Outcome.Failed(
              s"`$written` is not on this checkout's command list, so nothing ran. Only the " +
                s"commands the run tool's description lists may run. $hint"
            )
          )
        )
      (
        refused("read", "a"),
        refused("git status | head"),
        refused("scripts", "check", "grit.x"),
        refused("bash", "scripts/check", "grit.x"),
        refused("python3", "-c", "print(1)"),
        refused("rg", "Turn"),
        refused("curl", "https://example.com")
      ) ==> (
        unlisted("read a", "`read` is a tool of its own, not a program: call `read` itself."),
        unlisted(
          "'git status | head'",
          "No shell runs here: `program` is one program's name, and pipes, `&&`, `;`, quotes " +
            "and redirects cannot run. Give `git` as `program`, its arguments in `args`, one " +
            "program per call."
        ),
        unlisted(
          "scripts check grit.x",
          "A program is one word with its path: give `scripts/check` as `program`."
        ),
        unlisted(
          "bash scripts/check grit.x",
          "Run it as a program of its own: `program` `scripts/check`, `args` [`grit.x`]. No " +
            "shell or interpreter runs here: one program per call, no pipes."
        ),
        unlisted(
          "python3 -c 'print(1)'",
          "No shell or interpreter runs here: `run` starts one listed program per call, with its " +
            "arguments: no pipes, no scripts, no `-c` lines. For what it was for, `search` finds " +
            "lines in files, `read` reads a file, `list` lists a directory, and `edit` and " +
            "`write` change files."
        ),
        unlisted(
          "rg Turn",
          "`search` finds the lines matching a pattern in files; `grep …` runs here too."
        ),
        unlisted(
          "curl https://example.com",
          "Only these programs run here: `git`, `grep`, `scripts/check`, each as the run tool's " +
            "description lists it."
        )
      )
    }

    test(
      "git's refusals name its listed forms: a push is a person's, a reset or a branch checkout restores"
    ) {
      val runs = Allowlisted.over(new Noting(scripts), refusing)
      val host = new CodingTests.Scripted()
      def said(args: String*): Vector[String] =
        settle(host, runs, "run", ujson.Obj("program" -> "git", "args" -> args)).toOption.toVector
          .collect { case (_, Outcome.Failed(why)) => why.split("may run. ").last }
      val older = "For a file as an older commit had it: `git restore --source <commit> <path>`."
      (
        said("push", "origin", "x"),
        said("reset", "--hard", "HEAD~1"),
        said("checkout", "HEAD~1", "--", "a"),
        said("branch", "-D", "x")
      ) ==> (
        Vector("Pushing is left to a person here: say in your reply that the branch is ready."),
        Vector(
          "`git reset` does not run here. To undo changes to a file: `git restore <path>` or " +
            s"`git checkout -- <path>`. To set work aside: `git stash`. $older"
        ),
        Vector(
          "`git checkout` runs here only as `git checkout -- <path>`, to restore files: this " +
            s"checkout stays on its branch. $older"
        ),
        Vector(
          "The git commands that run here: `git status …`, `git restore …`, `git checkout -- …`, " +
            "`git stash …`."
        )
      )
    }

    test(
      "a refusal repeated this turn says so, a script's by the script in whatever form, a guard's word for word; a new turn starts the count again"
    ) {
      val runs = Allowlisted.over(new Noting(scripts), refusing)
      val host = new CodingTests.Scripted()
      val refusals = Refusals.kept()
      val guard = GuardFixtures.guard(Left("JEV_API_KEY is not set"))
      val later = TestCallSlots.at(
        grit.core.id.TurnRef(grit.core.id.ConversationId("c"), grit.core.id.TurnSeq.First.next)
      )
      def ran(at: grit.core.id.CallSlot, program: String, args: String*): Option[String] =
        settle(
          host,
          runs,
          "run",
          ujson.Obj("program" -> program, "args" -> args),
          guard,
          refusals,
          at
        ).toOption.collect { case (_, Outcome.Failed(why)) => why }
      val first = TestCallSlots.First
      def unlisted(written: String, after: String = "") =
        Some(
          s"`$written` is not on this checkout's command list, so nothing ran. Only the " +
            "commands the run tool's description lists may run. `run` starts only the listed " +
            "programs; no form of starting `scripts/kb-delta` is one, through `bash`, `sh` or any " +
            "other program. Do not retry it. If your task needs what it does, say so in your " +
            s"reply and go on without it.$after"
        )
      def again(n: Int) =
        s" This is refusal $n of `scripts/kb-delta` this turn: no form of it runs here, so stop " +
          "trying it and go on with what does."
      val branch = ran(first, "git", "stash", "branch", "x")
      (
        ran(first, "scripts/kb-delta"),
        ran(first, "bash", "scripts/kb-delta", "x"),
        ran(first, "sh", "-c", "scripts/kb-delta delta"),
        ran(later, "scripts/kb-delta"),
        ran(first, "git", "stash", "branch", "x")
      ) ==> (
        unlisted("scripts/kb-delta"),
        unlisted("bash scripts/kb-delta x", again(2)),
        unlisted("sh -c 'scripts/kb-delta delta'", again(3)),
        unlisted("scripts/kb-delta"),
        branch.map(_ + RunRefusals.Again)
      )
      assert(branch.exists(_.startsWith(MachineGuard.Lead)))
    }

    test(
      "a read of a source answers after the line naming its tools when the classifier says yes at Points, by rule when none answers, and not under it, of another file or with no source tools"
    ) {
      val cat = argv("cat", "a/B.scala")
      val readme = argv("cat", "README.md")
      val listing = Allowlist
        .of(Vector(Allowlist.Rule.Leading(argv("cat"))), 1.minute)
        .fold(e => throw new java.lang.AssertionError(e), identity)
      val shown = Map(
        cat -> ScriptedRuns.Script("object B\n", "", Some(Duration.Zero), 0),
        readme -> ScriptedRuns.Script("# x\n", "", Some(Duration.Zero), 0)
      )
      val scala = SourceTools
        .of(Sources.Scala, "`show` for a definition", "`uses` for every use")
        .toOption
      val host = new CodingTests.Scripted()
      // `answer` the classifier's p(yes), `None` with no classifier: the outcome, and how many
      // requests the classifier was sent.
      def read(
          command: Argv,
          answer: Option[Double],
          sources: Option[SourceTools] = scala
      ): (Option[Outcome], Int) = {
        val jev = new GuardFixtures.Judging(Right(answer.toVector))
        val runs = Allowlisted.over(new Noting(shown), listing)
        val outcome = settle(
          host,
          runs,
          "run",
          ujson.Obj("program" -> command.program, "args" -> command.args),
          GuardFixtures.guard(
            if (answer.isDefined) Right(jev) else Left("JEV_API_KEY is not set")
          ),
          sources = sources
        ).toOption.map(_._2)
        (outcome, jev.states.size)
      }
      val line = "Answered in fewer tokens by `show` for a definition.\n"
      val body = "Exit code 0. Output:\nobject B\n"
      (
        read(cat, Some(0.7)),
        read(cat, Some(0.69)),
        read(cat, None),
        read(readme, Some(0.9)),
        read(cat, Some(0.9), None)
      ) ==> (
        (Some(Outcome.Done(line + body)), 1),
        (Some(Outcome.Done(body)), 1),
        (Some(Outcome.Done(line + body)), 0),
        (Some(Outcome.Done("Exit code 0. Output:\n# x\n")), 0),
        (Some(Outcome.Done(body)), 0)
      )
    }

    test("run's description lists every command the allowlist permits, and how long it waits") {
      val run = described("run")
      assert(
        run.exists(
          _.endsWith(
            "The commands it runs, where `…` stands for any further arguments:\n" +
              "- `git status`\n- `git commit …`\n- `./mill …`\n" +
              "- `scripts/check …` (at least one argument: the suites or modules to check)"
          )
        ),
        run.exists(
          _.contains(
            "Waits up to `wait` seconds (30 when not given): a command still running then keeps " +
              "running, and the answer gives its id and its output so far; follow it with " +
              "`tail`, or end it with `stop`. At most 4 commands run at once in a checkout. " +
              "Fails, running nothing, when the command is not on the list or 4 are already " +
              "running, and when it cannot start; a refusal says what runs instead, and when it " +
              "was refused before this turn. Before it starts, a command that is not one " +
              "of the reads the check clears by itself, with every argument inside the " +
              "checkout, is checked for harm to this machine beyond the checkout, and refused, " +
              "running nothing, if it would change files outside it, rewrite what other " +
              "checkouts share, stop other processes, or change another machine. A command is " +
              "killed with everything it " +
              "started when it runs past `timeout` (at most 120 seconds, and that when not " +
              "given) or its output passes 16 MB."
          )
        ),
        run.exists(
          _.contains(
            "A non-zero exit is returned, not an error. An output over 8 KB is answered with " +
              "its size, its lines that look like errors, warnings or failing tests, each after " +
              "its line number, and up to its last 40 lines. All of it is in a file in the " +
              "checkout the answer names, for `read` and `search`, kept until 8 newer commands " +
              "have ended. Give `git commit` its message"
          )
        ),
        run.exists(
          _.contains(
            "Give `git commit` its message with `-m`: no editor can open. A `git commit`, " +
              "`--amend` included, is refused, committing nothing, while a file git neither " +
              "tracks nor ignores is in the checkout, or a tracked file has changes not staged " +
              "(with `-a`, only the first); then, where the repository declares a compile, it " +
              "runs that first and is refused when it fails. Either refusal lists what to fix. " +
              "Waits up to"
          )
        )
      )
    }

    test("run's description names the reads its list admits as held inside the checkout") {
      def describing(rules: Allowlist.Rule*): Vector[String] =
        Coding
          .allowlistedHosted(
            Allowlist
              .of(rules.toVector, 1.minute)
              .fold(e => throw new java.lang.AssertionError(e), identity)
          )
          .map(_.entry)
          .collect { case e if ToolName.value(e.name) == "run" => e.does }
      val reads = describing(
        Allowlist.Rule.Leading(argv("cat")),
        Allowlist.Rule.Following(argv("sed", "-n"), "what to print"),
        Allowlist.Rule.Leading(argv("git", "diff"))
      )
      val none = describing(Allowlist.Rule.Leading(argv("./mill")))
      val said =
        "or change another machine. A read (`git diff`, `cat` or `sed -n`) is refused, running " +
          "nothing, when an argument leads outside the checkout or names a secrets file (.env or " +
          ".env.*). A command is killed"
      (reads.map(_.contains(said)), none.map(_.contains("A read ("))) ==> (
        Vector(true),
        Vector(false)
      )
    }

    test("run, tail and stop are shown as their name, then the command or the id") {
      val box = Toolbox
        .of(Coding.allowlistedHosted(allowlist(10.minutes))*)
        .fold(d => throw new java.lang.AssertionError(d), identity)
      Vector(
        "run" -> ujson.Obj("program" -> "./mill", "args" -> ujson.Arr("compile")),
        "tail" -> ujson.Obj("id" -> nth(1), "wait" -> 5),
        "stop" -> ujson.Obj("id" -> nth(1))
      ).map((name, args) =>
        box.bind(TestTainted.authority(call(name, args)), Repairs.All).map(_.shown)
      ) ==> Vector(
        Right("run ./mill compile"),
        Right(s"tail ${nth(1)}"),
        Right(s"stop ${nth(1)}")
      )
    }

    test("tail and stop say what they take and how they fail") {
      // What the model reads, word for word.
      (described("tail"), described("stop")) ==> (
        Vector(
          "See how a command `run` left running stands: still running, with its output so far, " +
            "or ended, with its exit code and output. Give `wait` to wait up to that many " +
            "seconds for it to end; the answer comes as soon as it does. Shows a long output as " +
            "`run` does. Fails when no command with that id is kept in this " +
            "checkout: grit restarted since it started, or it ended and was forgotten after 8 " +
            "newer commands ended; and when its output passed 16 MB, which killed it."
        ),
        Vector(
          "End a command `run` left running, killing it and everything it started, and return " +
            "its output until then, a long one shown as `run` shows it. A command that already " +
            "ended is left as it was, and its exit code and output returned. Fails as `tail` does when no command with that id " +
            "is kept."
        )
      )
    }

    test("an allowlisted commit is logged with its sha and subject; another command as a run") {
      val made = "Exit code 0. Output:\n[m20 1a2b3c4] turn: a thing\n 1 file changed"
      def git(args: String*) = ujson.Obj("program" -> "git", "args" -> ujson.Arr.from(args))
      (
        logs("run", git("commit", "-m", "turn: a thing"), made),
        logs("run", git("status"), made)
      ) ==> (
        Event.Shape
          .committed(worktree, "1a2b3c4", Some("turn: a thing"))
          .map(s => Some(Event.Unrecorded.Commit(s))),
        Right(Some(Event.Unrecorded.Ran(Event.Shape.Empty)))
      )
    }

    test("a commit tail or stop sees end is logged as run logs one; any other answer, nothing") {
      val made = "Exit code 0. Output:\n[m20 1a2b3c4] turn: a thing\n 1 file changed"
      val id = ujson.Obj("id" -> nth(1))
      val commit = Event.Shape
        .committed(worktree, "1a2b3c4", Some("turn: a thing"))
        .map(s => Some(Event.Unrecorded.Commit(s)))
      (
        logs("tail", id, s"`git commit -m 'turn: a thing'`\n$made"),
        logs("stop", id, s"`git commit -m 'turn: a thing'`\n$made"),
        logs("tail", id, s"`./mill compile`\n$made")
      ) ==> (commit, commit, Right(None))
    }

    test(
      "a commit that would leave out files is refused, starting nothing, naming the untracked " +
        "then the unstaged, at most 20 of each and how many more"
    ) {
      val noted = new Noting(Map(argv("git", "commit", "-m", "x") -> instant))
      val runs = Allowlisted.over(noted, allowlist(2.minutes))
      val unstaged = (1 to 22).map(n => f"src/f$n%02d.scala").toVector
      val tree = Leaving.of(unstaged, Vector("new/Test.scala", "notes.md"))
      val refused =
        "The commit was refused, and nothing was committed: it would leave out files the " +
          "checkout holds. Untracked, neither committed nor ignored:\n" +
          "- new/Test.scala\n- notes.md\n" +
          "Changed, not staged:\n" +
          unstaged.take(20).map(p => s"- $p\n").mkString +
          "- … and 2 more\n\n" +
          "Add each with `git add`, or remove it, or ignore it in `.gitignore`; then commit again."
      (commit(runs, tree, "commit", "-m", "x"), noted.log, tree.reads.get) ==> (
        Right((false, Outcome.Failed(refused))),
        Vector.empty,
        1
      )
    }

    test(
      "a commit with -a or --all, or -a in a cluster before a valued flag, is refused only for " +
        "untracked files; any other commit, --amend included, for unstaged ones too"
    ) {
      val forms = Vector(
        Vector("commit", "-am", "x"),
        Vector("commit", "--all", "-m", "x"),
        Vector("commit", "-m", "x"),
        Vector("commit", "--amend", "--no-edit"),
        Vector("commit", "-ma", "x")
      )
      val noted = new Noting(forms.map(f => argv(("git" +: f)*) -> instant).toMap)
      val runs = Allowlisted.over(noted, allowlist(2.minutes))
      val unstagedOnly = Leaving.of(Vector("a.txt"), Vector.empty)
      val refused =
        "The commit was refused, and nothing was committed: it would leave out files the " +
          "checkout holds. Changed, not staged:\n- a.txt\n\n" +
          "Add each with `git add`, or remove it, or ignore it in `.gitignore`; then commit again."
      (
        forms.map(f => commit(runs, unstagedOnly, f*).map(_._2.isInstanceOf[Outcome.Done])),
        commit(runs, unstagedOnly, "commit", "--amend", "--no-edit"),
        noted.log.map(_._1)
      ) ==> (
        Vector(Right(true), Right(true), Right(false), Right(false), Right(false)),
        Right((false, Outcome.Failed(refused))),
        Vector(argv("git", "commit", "-am", "x"), argv("git", "commit", "--all", "-m", "x"))
      )
    }

    test("a commit the machine guard refuses fails in its words, the tree never read") {
      val noted = new Noting(Map(argv("git", "commit", "-m", "x", "../elsewhere") -> instant))
      val runs = Allowlisted.over(noted, allowlist(2.minutes))
      val jev = new GuardFixtures.Judging(Right(Vector(0.0, 0.0, 0.0, 0.9)))
      val tree = Leaving.of(Vector("a.txt"), Vector.empty)
      val ran = settle(
        new CodingTests.Scripted(),
        runs,
        "run",
        ujson.Obj("program" -> "git", "args" -> ujson.Arr("commit", "-m", "x", "../elsewhere")),
        GuardFixtures.guard(Right(jev)),
        tree = tree
      )
      (ran.map(_._2.toString.contains("the check that guards this machine")), tree.reads.get) ==>
        (Right(true), 0)
    }

    test("a commit whose tree git cannot read is refused, failing closed, in the error's words") {
      val noted = new Noting(Map(argv("git", "commit", "-m", "x") -> instant))
      val runs = Allowlisted.over(noted, allowlist(2.minutes))
      val unread = new Leaving(Left(HostError.Failed("git status exited 128: not a repository")))
      (commit(runs, unread, "commit", "-m", "x"), noted.log) ==> (
        Right(
          (
            false,
            Outcome.Failed(
              "The commit was refused, and nothing was committed: the working tree could not " +
                "be checked. git status exited 128: not a repository"
            )
          )
        ),
        Vector.empty
      )
    }

    test("a commit's flags are read for -a as git reads them, a valued flag's value never one") {
      def all(args: String*) = CommitCheck.all(argv(("git" +: args)*))
      Vector(
        all("commit", "-a"),
        all("commit", "-qa", "-m", "x"),
        all("-C", "sub", "commit", "--all"),
        all("commit", "-m", "-a"),
        all("commit", "--message", "-a"),
        all("commit", "-ma"),
        all("commit", "-Ca"),
        all("commit", "--", "-a"),
        all("commit", "--amend"),
        all("status", "-a")
      ) ==> Vector(true, true, true, false, false, false, false, false, false, false)
    }

    test(
      "with a compile declared, a commit whose tree leaves nothing out runs it first, the guard " +
        "not asked, and commits when it exits 0; with none declared, commits at once"
    ) {
      val noted = new Noting(Map(build -> instant, committing -> instant))
      val runs = Allowlisted.over(noted, allowlist(2.minutes))
      val jev = new GuardFixtures.Judging(Right(Vector(0.0, 0.0, 0.0, 0.0)))
      val declared = settle(
        new CodingTests.Scripted(),
        runs,
        "run",
        ujson.Obj("program" -> "git", "args" -> ujson.Arr("commit", "-m", "x")),
        GuardFixtures.guard(Right(jev)),
        compile = Some(build)
      )
      val first = noted.log
      val undeclared = commit(runs, Leaving.Nothing, "commit", "-m", "x")
      (declared, undeclared, first, noted.log.drop(first.size), jev.states.size) ==> (
        Right((false, Outcome.Done("Exit code 0; no output."))),
        Right((false, Outcome.Done("Exit code 0; no output."))),
        Vector((build, 2.minutes), (committing, 2.minutes)),
        Vector((committing, 2.minutes)),
        0
      )
    }

    test("a commit whose compile fails is refused with its exit and output, committing nothing") {
      val noted = new Noting(
        Map(
          build -> ScriptedRuns
            .Script("[error] Foo.scala:3: not found: value bar\n", "", Some(1.second), 1),
          committing -> instant
        )
      )
      val runs = Allowlisted.over(noted, allowlist(2.minutes))
      (
        compiling(runs, Leaving.Nothing, Some(build), "commit", "-m", "x"),
        noted.log.map(_._1)
      ) ==> (
        Right(
          (
            false,
            Outcome.Failed(
              "The commit was refused, and nothing was committed: `./mill __.compile` failed on " +
                "the working tree (exit 1). Fix what it reports, then commit again. Output:\n" +
                "[error] Foo.scala:3: not found: value bar\n"
            )
          )
        ),
        Vector(build)
      )
    }

    test("a commit whose compile runs past its timeout is refused, committing nothing") {
      val noted = new Noting(
        Map(build -> ScriptedRuns.Script("compiling\n", "", None, 0), committing -> instant)
      )
      val runs = Allowlisted.over(noted, allowlist(2.minutes))
      (compiling(runs, Leaving.Nothing, Some(build), "commit", "-m", "x"), noted.log) ==> (
        Right(
          (
            false,
            Outcome.Failed(
              "The commit was refused, and nothing was committed: `./mill __.compile` ran past " +
                "120 seconds and was killed. Its output until then:\ncompiling\n"
            )
          )
        ),
        Vector((build, 2.minutes))
      )
    }

    test(
      "a commit that would leave out a file starts no compile; one whose compile cannot start " +
        "is refused, failing closed, in the error's words"
    ) {
      val noted = new Noting(Map(committing -> instant))
      val runs = Allowlisted.over(noted, allowlist(2.minutes))
      val untracked = Leaving.of(Vector.empty, Vector("new.txt"))
      (
        compiling(runs, untracked, Some(build), "commit", "-m", "x").map(
          _._2.toString.contains("it would leave out files")
        ),
        compiling(runs, Leaving.Nothing, Some(build), "commit", "-m", "x"),
        noted.log
      ) ==> (
        Right(true),
        Right(
          (
            false,
            Outcome.Failed(
              "The commit was refused, and nothing was committed: `./mill __.compile` could not " +
                "be checked. ./mill: not found"
            )
          )
        ),
        Vector((build, 2.minutes))
      )
    }
  }
}
