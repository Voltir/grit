package grit.eval.harness.evidence

import scala.concurrent.duration.DurationInt

import grit.core.host.RelPath
import grit.core.id.{Harness, SessionId}
import grit.core.message.{Tokens, Usage}
import grit.core.model.ModelId
import grit.dbos.internal.TrialRows
import grit.eval.harness.bench.{Checked, PerPass, Standing, Tried}

import utest.*

/** An evidence checkout's files scanned for the bench's grade shapes ([[Shown]]), each shape
  * written by the bench's own writer of it.
  */
object ShownTests extends TestSuite {

  private def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), identity)

  private def rel(s: String): RelPath =
    RelPath.of(s).fold(e => throw new java.lang.AssertionError(e.toString), identity)

  /** Trial `id` of grit on `task`, passed or not, at `distance`. */
  private def trial(id: Int, task: String, passed: Boolean, distance: Option[Long]) =
    got(
      TrialRows.Trial.of(
        got(SessionId.of(got(Harness.of("grit")), s"s$id")),
        got(TrialRows.TaskName.of(task)),
        ModelId.of("anthropic/claude-opus-5-5").getOrElse(throw new java.lang.AssertionError()),
        Usage(Tokens(100), Tokens(10), Tokens.Zero, Some(BigDecimal("0.5"))),
        3,
        1.second,
        None,
        None,
        passed,
        distance,
        None
      )
    )

  private val trials: Vector[TrialRows.Trial] =
    Vector(trial(1, "a-task", passed = true, Some(12)), trial(2, "a-task", passed = false, None))

  /** A coder's trajectory: what it ran, and what it saw of its own tests and checks. */
  private val trajectory = Vector(
    "**2 · assistant**",
    "- calls `run` (`t1`) with `{\"command\":\"scripts/check grit.core\"}`",
    "**3 · tool result** for `t1`",
    "```text",
    "unit: 430 passed, 0 failed (1 module)",
    "Tests: 12, Passed: 12, Failed: 0",
    "```",
    "The tests pass, and all checks green.",
    "passed: the suite on the second try"
  ).mkString("\n")

  val tests = Tests {
    test("each grade shape the bench writes is refused, naming the file and the shape") {
      val files = Vector(
        rel("trials/1.md") -> ("ended: done\n" + Tried.verdict(trials(0))),
        rel("trials/2.md") -> Tried.verdict(trials(1)),
        rel("brief.md") -> "Make the change.",
        rel("checks.md") ->
          Checked(Vector("check grit.core" -> true), None).lines.mkString("\n"),
        rel("stopped.md") ->
          Checked(Vector.empty, Some("the cap")).lines.mkString("\n"),
        rel("summary.md") -> PerPass.lines(trials).mkString("\n"),
        rel("rows.md") -> PerPass.lines(trials).drop(1).mkString("\n")
      )
      Shown.of(files) ==> Left(
        Vector(
          GradeFound(rel("trials/1.md"), Vector(GradeShape.Verdict)),
          GradeFound(rel("trials/2.md"), Vector(GradeShape.Verdict)),
          GradeFound(rel("checks.md"), Vector(GradeShape.CheckOutcome)),
          GradeFound(rel("stopped.md"), Vector(GradeShape.CheckOutcome)),
          GradeFound(rel("summary.md"), Vector(GradeShape.Summary)),
          GradeFound(rel("rows.md"), Vector(GradeShape.Summary))
        )
      )
    }
    test("a file holding several grade shapes is named once, with each shape in order") {
      val text = (Tried.verdict(trials(0)) +: PerPass.lines(trials)).mkString("\n")
      Shown.of(Vector(rel("all.md") -> text)) ==>
        Left(Vector(GradeFound(rel("all.md"), Vector(GradeShape.Verdict, GradeShape.Summary))))
    }
    test("a coder's own test output and its word that the checks pass are let through whole") {
      val files = Vector(rel("trajectory.md") -> trajectory, rel("brief.md") -> "Do it.")
      Shown.of(files).map(_.files) ==> Right(files)
    }
    test(
      "a standing text holding a grade shape is refused under standing.md, and one without is kept"
    ) {
      val clean = got(Standing.of("Run the checks before you stop."))
      val graded = got(Standing.of(s"A good run looks like:\n${Tried.verdict(trials(0))}"))
      (Shown.scanned(clean), Shown.scanned(graded)) ==> (
        Right(clean),
        Left(GradeFound(rel("standing.md"), Vector(GradeShape.Verdict)))
      )
    }
  }
}
