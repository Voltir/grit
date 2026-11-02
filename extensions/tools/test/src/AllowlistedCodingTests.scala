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
          Allowlist.Rule.Leading(argv("./mill"))
        ),
        longest
      )
      .fold(e => throw new java.lang.AssertionError(e), identity)

  /** What the scripted commands do: `git status` ends at once, `./mill compile` after two
    * minutes, and `./mill hang` and `./mill quiet`, which writes nothing, never.
    */
  private val scripts = Map(
    argv("git", "status") -> ScriptedRuns.Script("ok\n", "", Some(Duration.Zero), 0),
    argv("./mill", "compile") -> ScriptedRuns.Script("compiling\n", "done\n", Some(2.minutes), 0),
    argv("./mill", "hang") -> ScriptedRuns.Script("started\n", "", None, 0),
    argv("./mill", "quiet") -> ScriptedRuns.Script("", "", None, 0)
  )

  /** The id the scripted runs give their `n`th run. */
  private def nth(n: Int): String = RunId.of(s"scripted-$n").written

  private def tools(
      host: CodingTests.Scripted^,
      runs: Allowlisted^
  ): Toolbox[caps.CapSet^{host, runs}] =
    Coding.allowlisted(host, host, runs).fold(d => throw new java.lang.AssertionError(d), identity)

  private def call(name: String, args: ujson.Value): AssistantBlock.ToolCall =
    AssistantBlock.ToolCall(ToolCallId("c1"), name, args)

  /** `name` called with `args` against the allowlisted tools: whether it asks, and what it came to
    * when run.
    */
  private def settle(
      host: CodingTests.Scripted,
      runs: Allowlisted^,
      name: String,
      args: ujson.Value
  ): Either[String, (Boolean, Outcome)] =
    tools(host, runs)
      .bind(TestTainted.authority(call(name, args)), Repairs.All)
      .left
      .map(_.message)
      .map {
        case free: Bound.Free => (false, free(TestCallSlots.First))
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
        Right((false, Outcome.Failed(HostError.Unlisted(argv("rm", "-rf", "a b")).message)))
      )
      noted.log ==> Vector((argv("git", "status"), 2.minutes))
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

    test("run's description lists every command the allowlist permits, and how long it waits") {
      val run = described("run")
      assert(
        run.exists(
          _.endsWith(
            "The commands it runs, where `…` stands for any further arguments:\n" +
              "- `git status`\n- `git commit …`\n- `./mill …`"
          )
        ),
        run.exists(
          _.contains(
            "Waits up to `wait` seconds (30 when not given): a command still running then keeps " +
              "running, and the answer gives its id and its output so far; follow it with " +
              "`tail`, or end it with `stop`. At most 4 commands run at once in a checkout. " +
              "Fails, running nothing, when the command is not on the list or 4 are already " +
              "running, and when it cannot start. A command is killed with everything it " +
              "started when it runs past `timeout` (at most 120 seconds, and that when not " +
              "given) or its output passes 16 MB."
          )
        )
      )
    }

    test("tail and stop say what they take and how they fail") {
      // What the model reads, word for word.
      (described("tail"), described("stop")) ==> (
        Vector(
          "See how a command `run` left running stands: still running, with its output so far, " +
            "or ended, with its exit code and output. Give `wait` to wait up to that many " +
            "seconds for it to end; the answer comes as soon as it does. Shows at most the last " +
            "2000 lines and 50 KB of output; when that cuts it, the answer names the file " +
            "holding all of it, for `read`. Fails when no command with that id is kept in this " +
            "checkout: grit restarted since it started, or it ended and was forgotten after 8 " +
            "newer commands ended; and when its output passed 16 MB, which killed it."
        ),
        Vector(
          "End a command `run` left running, killing it and everything it started, and return " +
            "its output until then. A command that already ended is left as it was, and its " +
            "exit code and output returned. Fails as `tail` does when no command with that id " +
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
  }
}
