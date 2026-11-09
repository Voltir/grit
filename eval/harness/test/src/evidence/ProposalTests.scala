package grit.eval.harness.evidence

import scala.concurrent.duration.DurationInt

import grit.core.host.RelPath
import grit.core.id.{Harness, SessionId}
import grit.core.message.{Tokens, Usage}
import grit.core.model.ModelId
import grit.dbos.internal.TrialRows
import grit.eval.harness.bench.{Bench, BenchTask, Standing, Tried}
import grit.eval.harness.sent.SentMarkdown

import utest.*

/** What a proposer reads ([[Proposal.of]]), and a kept trajectory as it is shown one
  * ([[Trajectory.within]]).
  */
object ProposalTests extends TestSuite {

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def rel(s: String): RelPath =
    RelPath.of(s).fold(e => throw new java.lang.AssertionError(e.toString), identity)

  private val task: BlindTask = BlindTask.of(
    got(
      BenchTask.of(
        got(TrialRows.TaskName.of("split-reader")),
        """{"base":"c905203b2f09fb367b42e0476b609fb458ec2bb4",
          |"commits":["8c5f8a48114526adebfbbbabe7217d2ec5478bc9"],
          |"checks":[{"runner":"check","args":["grit.models"]}]}""".stripMargin,
        "Split the reader in two."
      )
    )
  )

  private val standing: Standing = got(Standing.of("Read before you edit."))

  private val against: Proposal.Against = got(Proposal.Against.of("2370"))

  private def session(local: String): SessionId = got(SessionId.of(got(Harness.of("grit")), local))

  private def run(local: String, text: String): Proposal.Run =
    Proposal.Run(session(local), Bench.Transcript(text, clipped = false))

  /** A kept trajectory's tool result for call `id`, as the bench's markdown writes it. */
  private def result(n: Int, id: String, content: String): Vector[String] =
    Vector("", s"**$n · tool result** for `$id`") ++ SentMarkdown.fenced(content, "text")

  /** A kept trajectory's assistant call of `run` with `command`. */
  private def call(n: Int, id: String, command: String): Vector[String] =
    Vector("", s"**$n · assistant**", s"- calls `run` (`$id`) with `{\"command\":\"$command\"}`")

  /** Trial `local` of `harness` on `task`, begun with the standing text `began`, passed at 12. */
  private def trial(
      local: String,
      task: String = "split-reader",
      harness: String = "grit",
      began: Option[Standing] = Some(standing)
  ): TrialRows.Trial =
    got(
      TrialRows.Trial.of(
        got(SessionId.of(got(Harness.of(harness)), local)),
        got(TrialRows.TaskName.of(task)),
        ModelId.of("anthropic/claude-opus-5-5").getOrElse(throw new java.lang.AssertionError()),
        Usage(Tokens(100), Tokens(10), Tokens.Zero, None),
        3,
        1.second,
        None,
        None,
        passed = true,
        Some(12),
        began.map(_.sha256)
      )
    )

  private val verdict: String = Tried.verdict(trial("graded"))

  /** Turn `seq`'s request as the renderer writes it: its system prompt, its tools and `messages`. */
  private def turn(seq: Int, system: String, tools: String, messages: Vector[Vector[String]]) =
    Vector(
      s"${SentMarkdown.TurnHeading}$seq of c1: the context it was sent",
      "",
      "## The request",
      ""
    ) ++
      (Vector(s"${SentMarkdown.SystemHeading}1 fragments: base/grit)", "") ++
        SentMarkdown.fenced(system, "text") :+ "") ++
      Vector(s"${SentMarkdown.ToolsHeading}recorded set `t`)", "", s"- `$tools`: does it.") ++
      (Vector("", s"${SentMarkdown.MessagesHeading}the last call's ${messages.size})") ++
        messages.flatten) ++ Vector("## Each call", "", "a call")

  /** Message `n`, a user's `text`, as the renderer writes it. */
  private def said(n: Int, text: String): Vector[String] =
    Vector("", s"**$n · user** (a message)") ++ SentMarkdown.fenced(text, "text")

  val tests = Tests {
    test("a turn's system prompt, tools and messages already shown are marked, not repeated") {
      val first = turn(
        0,
        "You are a coder.\n### Tools offered (inside)",
        "read",
        Vector(said(1, "Brief."), call(2, "t1", "ls"))
      )
      val second = turn(
        1,
        "You are a coder.\n### Tools offered (inside)",
        "read",
        Vector(said(1, "Brief."), call(2, "t1", "ls"), result(3, "t1", "a.txt"))
      )
      Trajectory.deduplicated((first ++ second).mkString("\n")) ==> (first ++ Vector(
        s"${SentMarkdown.TurnHeading}1 of c1: the context it was sent",
        "",
        "## The request",
        "",
        s"${SentMarkdown.SystemHeading}1 fragments: base/grit)",
        "(the same as turn 0's system prompt, shown above)",
        "",
        s"${SentMarkdown.ToolsHeading}recorded set `t`)",
        "(the same as turn 0's tools, shown above)",
        "",
        s"${SentMarkdown.MessagesHeading}the last call's 3)",
        "",
        "**1 · user** (a message)",
        "(the same as turn 0's message 1, shown above)",
        "",
        "**2 · assistant**",
        "(the same as turn 0's message 2, shown above)"
      ) ++ result(3, "t1", "a.txt") ++ Vector("## Each call", "", "a call")).mkString("\n")
    }
    test("a message that differs is kept whole") {
      val first =
        turn(0, "You are a coder.", "read", Vector(said(1, "Brief."), call(2, "t1", "ls")))
      val second = turn(
        1,
        "You are a tester.",
        "write",
        Vector(said(1, "Brief, amended."), call(2, "t1", "ls -a"))
      )
      val kept = (first ++ second).mkString("\n")
      (Trajectory.deduplicated(kept), Trajectory.turns(kept)) ==> (kept, 2)
    }
    test(
      "a tool result past the cut is clipped with the mark, and the lines around it are kept whole"
    ) {
      val long = "a ``` inside\n" + ("x" * 30)
      val before = Vector("# Run", "a line of the run") ++ call(2, "t1", "cat a")
      val kept =
        (before ++ result(3, "t1", long) ++ call(4, "t2", "ls") ++ result(5, "t2", "short"))
          .mkString("\n")
      Trajectory.within(kept, 20) ==> (
        before ++ Vector("", "**3 · tool result** for `t1`") ++ SentMarkdown.within(
          long,
          20,
          None
        ) ++
          call(4, "t2", "ls") ++ result(5, "t2", "short")
      ).mkString("\n")
    }
    test("a coder's own 'tests pass' line is let through") {
      val text = (call(2, "t1", "scripts/check grit.core") ++
        result(3, "t1", "unit: 430 passed, 0 failed (1 module)") ++
        Vector("", "The tests pass, and all checks green.")).mkString("\n")
      val shown =
        Proposal.of(task, Vector(run("r1", text)), standing, against, Vector.empty, "Propose.")
      shown.map(_.files.toMap.get(rel("trajectories/1.md"))) ==>
        Right(Some(s"Run 1, session r1.\n\n$text"))
    }
    test("each file at its path") {
      val shown = Proposal.of(
        task,
        Vector(run("r1", "one"), run("r2", "two")),
        standing,
        against,
        Vector("an amendment", "another"),
        "Propose."
      )
      shown.map(_.files.map(_._1)) ==> Right(
        Vector(
          Proposal.Said,
          rel("task.md"),
          rel("standing.md"),
          rel("trajectories/1.md"),
          rel("trajectories/2.md"),
          rel("amendments/1.md"),
          rel("amendments/2.md")
        )
      )
    }
    test("an amendment holding a trial's verdict line refuses, naming it") {
      Proposal.of(
        task,
        Vector(run("r1", "one")),
        standing,
        against,
        Vector("fine", s"it did: $verdict"),
        "Propose."
      ) ==>
        Left(Vector(GradeFound(rel("amendments/2.md"), Vector(GradeShape.Verdict))))
    }
    test(
      "named trials are chosen in the order named, one kept before standing texts were recorded among them; trials of two tasks are refused, naming each"
    ) {
      val kept =
        Vector(
          trial("a"),
          trial("b"),
          trial("unrecorded", began = None),
          trial("c", task = "other-task")
        )
      (
        Proposal.chosen(kept, Vector("b", "unrecorded", "a"), standing),
        Proposal.chosen(kept, Vector("a", "c"), standing)
      ) ==> (
        Right(Vector(trial("b"), trial("unrecorded", began = None), trial("a"))),
        Left("the trials are of more than one task: split-reader, other-task")
      )
    }
    test(
      "a trial is refused, naming it, when none or two are named by its id, it is named twice, or it is not grit's or began with another standing text"
    ) {
      val other = got(Standing.of("Something else."))
      val kept = Vector(
        trial("a"),
        trial("cc", harness = "claude-code", began = None),
        trial("old", began = Some(other)),
        trial("both"),
        trial("both", harness = "claude-code")
      )
      Vector(
        Vector(),
        Vector("missing"),
        Vector("a", "a"),
        Vector("cc"),
        Vector("old"),
        Vector("both")
      )
        .map(Proposal.chosen(kept, _, standing)) ==> Vector(
        Left("no trial is named"),
        Left("trial missing is no trial the bench keeps"),
        Left("trial a is named twice"),
        Left("trial cc is not grit's, so it began with no standing text"),
        Left("trial old began with another standing text than the one given"),
        Left("trial both names more than one trial, of different harnesses")
      )
    }
    test("the version a standing text is from is a whole number of 1 or more") {
      Vector("2370", "0", "-3", "v12", "").map(
        Proposal.Against.of(_).map(Proposal.Against.version)
      ) ==>
        Vector(
          Right(2370L),
          Left("--against 0: not a version, a whole number of 1 or more"),
          Left("--against -3: not a version, a whole number of 1 or more"),
          Left("--against v12: not a version, a whole number of 1 or more"),
          Left("--against : not a version, a whole number of 1 or more")
        )
    }
    test("the said text is the brief, then the files") {
      val shown = Proposal.of(
        task,
        Vector(run("r1", "one")),
        standing,
        against,
        Vector.empty,
        "Propose an amendment.\n"
      )
      shown.map(_.files.headOption) ==> Right(
        Some(
          Proposal.Said ->
            """Propose an amendment.
              |
              |## The evidence
              |
              |Each path is under the directory you work in.
              |
              |- `base/`: the repository the coder started from, at the commit the task names.
              |- `evidence/task.md`: the task, as its coder was given it.
              |- `evidence/standing.md`: the standing text every run began with, its section's text at version 2370, the version an amendment's Diff is against.
              |- `evidence/trajectories/1.md`: one run of the task, each model call in order. A tool result longer than 4,000 characters is cut there, with a line saying how much was left out.
              |- No amendment has been proposed before.
              |""".stripMargin
        )
      )
    }
  }
}
