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

  private def session(local: String): SessionId = got(SessionId.of(got(Harness.of("grit")), local))

  private def run(local: String, text: String): Proposal.Run =
    Proposal.Run(session(local), Bench.Transcript(text, clipped = false))

  /** A kept trajectory's tool result for call `id`, as the bench's markdown writes it. */
  private def result(n: Int, id: String, content: String): Vector[String] =
    Vector("", s"**$n · tool result** for `$id`") ++ SentMarkdown.fenced(content, "text")

  /** A kept trajectory's assistant call of `run` with `command`. */
  private def call(n: Int, id: String, command: String): Vector[String] =
    Vector("", s"**$n · assistant**", s"- calls `run` (`$id`) with `{\"command\":\"$command\"}`")

  private val verdict: String = Tried.verdict(
    got(
      TrialRows.Trial.of(
        session("graded"),
        got(TrialRows.TaskName.of("split-reader")),
        ModelId.of("anthropic/claude-opus-5-5").getOrElse(throw new java.lang.AssertionError()),
        Usage(Tokens(100), Tokens(10), Tokens.Zero, None),
        3,
        1.second,
        None,
        None,
        passed = true,
        Some(12),
        None
      )
    )
  )

  val tests = Tests {
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
      val shown = Proposal.of(task, Vector(run("r1", text)), standing, Vector.empty, "Propose.")
      shown.map(_.files.toMap.get(rel("evidence/trajectories/1.md"))) ==>
        Right(Some(s"Run 1, session r1.\n\n$text"))
    }
    test("each file at its path") {
      val shown = Proposal.of(
        task,
        Vector(run("r1", "one"), run("r2", "two")),
        standing,
        Vector("an amendment", "another"),
        "Propose."
      )
      shown.map(_.files.map(_._1)) ==> Right(
        Vector(
          Proposal.Said,
          rel("evidence/task.md"),
          rel("evidence/standing.md"),
          rel("evidence/trajectories/1.md"),
          rel("evidence/trajectories/2.md"),
          rel("evidence/amendments/1.md"),
          rel("evidence/amendments/2.md")
        )
      )
    }
    test("an amendment holding a trial's verdict line refuses, naming it") {
      Proposal.of(
        task,
        Vector(run("r1", "one")),
        standing,
        Vector("fine", s"it did: $verdict"),
        "Propose."
      ) ==>
        Left(Vector(GradeFound(rel("evidence/amendments/2.md"), Vector(GradeShape.Verdict))))
    }
    test("the said text is the brief, then the files") {
      val shown = Proposal.of(
        task,
        Vector(run("r1", "one")),
        standing,
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
              |Each path is under this directory.
              |
              |- `base/`: the repository the coder started from, at the commit the task names.
              |- `evidence/task.md`: the task, as its coder was given it.
              |- `evidence/standing.md`: the standing text every run began with.
              |- `evidence/trajectories/1.md`: one run of the task, each model call in order. A tool result longer than 4,000 characters is cut there, with a line saying how much was left out.
              |- No amendment has been proposed before.
              |""".stripMargin
        )
      )
    }
  }
}
