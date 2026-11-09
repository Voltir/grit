package grit.eval.harness.evidence

import grit.core.host.RelPath
import grit.core.id.SessionId
import grit.eval.harness.bench.{Bench, Sha, Standing}
import grit.eval.harness.sent.Cut

/** What a proposer of amendments to the coder's standing text reads: one task's blind brief, the
  * standing text its runs began with, the runs as the bench kept them, and every amendment
  * proposed before; and the text it is said, which points at them.
  */
object Proposal {

  /** The file a proposer is said: `PROPOSE.md`, at the checkout's root. */
  val Said: RelPath = path("PROPOSE.md")

  /** The most characters of one tool result a run's file shows: [[Cut.Default]]'s. */
  val ResultChars: Int = Cut.Default match {
    case Cut.Within(chars) => chars
    case Cut.Whole => Int.MaxValue
  }

  /** A run a proposer reads: its session, and its transcript as the bench kept it. */
  final case class Run(session: SessionId, kept: Bench.Transcript) extends caps.Pure

  /** `task`'s proposal: [[Said]], `brief` then the list of what follows; `evidence/task.md`, the
    * task's brief; `evidence/standing.md`, `standing`; each of `runs` at
    * `evidence/trajectories/<n>.md`, numbered from 1 in order, a line naming its session first
    * (and saying so when the bench clipped it), then its transcript with each tool result cut to
    * [[ResultChars]] ([[Trajectory.within]]); and each of `amendments`, an amendment's text as
    * exported, at `evidence/amendments/<n>.md`, numbered from 1 in order. Or each file holding a
    * grade shape ([[Shown.of]]), an amendment among them.
    */
  def of(
      task: BlindTask,
      runs: Vector[Run],
      standing: Standing,
      amendments: Vector[String],
      brief: String
  ): Either[Vector[GradeFound], Shown] = {
    val trajectories = runs.zipWithIndex.map { (run, i) =>
      val named = s"Run ${i + 1}, session ${run.session.local}." +
        (if (run.kept.clipped) s" $Clipped" else "")
      path(s"evidence/trajectories/${i + 1}.md") ->
        s"$named\n\n${Trajectory.within(run.kept.text, ResultChars)}"
    }
    val earlier =
      amendments.zipWithIndex.map((text, i) => path(s"evidence/amendments/${i + 1}.md") -> text)
    val listed = Vector(
      "- `base/`: the repository the coder started from, at the commit the task names.",
      "- `evidence/task.md`: the task, as its coder was given it.",
      "- `evidence/standing.md`: the standing text every run began with."
    ) ++ trajectories.map((p, _) =>
      s"- `${RelPath.value(p)}`: one run of the task, each model call in order. A tool result " +
        f"longer than $ResultChars%,d characters is cut there, with a line saying how much was " +
        "left out."
    ) ++ (
      if (earlier.isEmpty) Vector("- No amendment has been proposed before.")
      else
        earlier.map((p, _) =>
          s"- `${RelPath.value(p)}`: an amendment proposed before, with what became of it."
        )
    )
    Shown.of(
      Vector(
        Said -> (brief.trim + "\n\n## The evidence\n\nEach path is under this directory.\n\n" +
          listed.mkString("", "\n", "\n")),
        path("evidence/task.md") -> (
          s"# The task: ${grit.dbos.internal.TrialRows.TaskName.value(task.name)}\n\n" +
            s"Its coder started from commit ${Sha.value(task.base)}. Its brief, as its coder " +
            s"was given it:\n\n${task.brief}"
        ),
        path("evidence/standing.md") -> standing.text
      ) ++ trajectories ++ earlier
    )
  }

  /** `found` for a person, the file it names called `name`: each grade shape it holds. */
  def graded(name: String, found: GradeFound): String =
    s"$name holds the bench's own grading text (${found.shapes.mkString(", ")}); " +
      "take it out: what is shown must not hold a grade"

  /** The line a run's file begins with when the bench cut its transcript. */
  private val Clipped: String =
    "This run's transcript was cut when it was kept, so its end is missing."

  /** `s` as a path: each this object makes, a literal or one with a number in it, is one. */
  private def path(s: String): RelPath =
    RelPath
      .of(s)
      .fold(e => throw new IllegalStateException(s"not a relative path: $s ($e)"), identity)
}
