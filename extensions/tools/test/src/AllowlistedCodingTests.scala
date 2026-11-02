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

/** [[Coding.allowlisted]]'s tools over a scripted checkout and noting processes: what asks, what
  * reaches the processes, and what the model is told and reads back.
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

  val tests = Tests {
    test("the engine offers the tools an edge runs: the same names, schemas, gates and retries") {
      val list = allowlist(2.minutes)
      Coding.allowlistedHosted(list).map(_.entry) ==>
        tools(new CodingTests.Scripted(), Allowlisted.over(new Noting, list)).set.tools
    }

    test(
      "none asks first; write, edit and run require member; every result is believed at member"
    ) {
      val list = allowlist(2.minutes)
      tools(new CodingTests.Scripted(), Allowlisted.over(new Noting, list)).set.tools.map(e =>
        (ToolName.value(e.name), e.asks, e.requires)
      ) ==> Vector(
        ("read", false, Integrity.Untrusted),
        ("list", false, Integrity.Untrusted),
        ("search", false, Integrity.Untrusted),
        ("write", false, Integrity.Member),
        ("edit", false, Integrity.Member),
        ("run", false, Integrity.Member)
      )
      Coding.allowlistedHosted(list).map(_.output).distinct ==> Vector(
        Believed.At(Integrity.Member)
      )
    }

    test(
      "a listed run runs unasked at the root; an unlisted one fails in Unlisted's words, running nothing"
    ) {
      val processes = new Noting(Ran(0, Clipped.tail("ok\n", _ => None)))
      val runs = Allowlisted.over(processes, allowlist(2.minutes))
      val host = new CodingTests.Scripted()
      (
        settle(host, runs, "run", ujson.Obj("program" -> "git", "args" -> ujson.Arr("status"))),
        settle(host, runs, "run", ujson.Obj("program" -> "rm", "args" -> ujson.Arr("-rf", "a b")))
      ) ==> (
        Right((false, Outcome.Done("Exit code 0. Output:\nok\n"))),
        Right((false, Outcome.Failed(HostError.Unlisted(argv("rm", "-rf", "a b")).message)))
      )
      processes.log ==> Vector((argv("git", "status"), RelPath.Root, 60.seconds))
    }

    test("write and edit run unasked") {
      val host = new CodingTests.Scripted()
      val runs = Allowlisted.over(new Noting, allowlist(2.minutes))
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

    test(
      "run's timeout is at most the allowlist's longest, and defaults to 60 s or that, the less"
    ) {
      val processes = new Noting
      val short = Allowlisted.over(processes, allowlist(30.seconds))
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
          Allowlisted.over(processes, allowlist(2.minutes)),
          "run",
          ujson.Obj.from(status.value ++ Seq("timeout" -> ujson.Num(90)))
        ).map(_._2)
      ) ==> (
        Right(Outcome.Done("Exit code 0; no output.")),
        true,
        Right(Outcome.Done("Exit code 0; no output."))
      )
      processes.log.map(_._3) ==> Vector(30.seconds, 90.seconds)
    }

    test("run's description lists every command the allowlist permits") {
      val described = Coding
        .allowlistedHosted(allowlist(2.minutes))
        .map(_.entry)
        .collect { case e if ToolName.value(e.name) == "run" => e.does }
      assert(
        described.exists(
          _.endsWith(
            "The commands it runs, where `…` stands for any further arguments:\n" +
              "- `git status`\n- `git commit …`\n- `./mill …`"
          )
        )
      )
    }

    test("an allowlisted commit is logged with its sha and subject; another command as a run") {
      val engine = Toolbox
        .of(Coding.allowlistedHosted(allowlist(2.minutes))*)
        .fold(d => throw new java.lang.AssertionError(d), identity)
      val worktree =
        Directory
          .of("/work/grit")
          .map(Place.of)
          .fold(e => throw new java.lang.AssertionError(e), identity)
      val made = "Exit code 0. Output:\n[m20 1a2b3c4] turn: a thing\n 1 file changed"
      def logs(args: String*) =
        engine
          .bind(
            TestTainted.authority(
              call("run", ujson.Obj("program" -> "git", "args" -> ujson.Arr.from(args)))
            ),
            Repairs.All
          )
          .map {
            case hosted: Bound.Hosted => hosted.logs(worktree, made)
            case _ => None
          }
      (logs("commit", "-m", "turn: a thing"), logs("status")) ==> (
        Event.Shape
          .committed(worktree, "1a2b3c4", Some("turn: a thing"))
          .map(s => Some(Event.Unrecorded.Commit(s))),
        Right(Some(Event.Unrecorded.Ran(Event.Shape.Empty)))
      )
    }
  }
}
