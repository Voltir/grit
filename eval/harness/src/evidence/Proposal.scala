package grit.eval.harness.evidence

import grit.core.host.RelPath
import grit.core.id.{Harness, SessionId}
import grit.dbos.internal.TrialRows
import grit.eval.harness.bench.{Bench, Sha, Standing}
import grit.eval.harness.sent.Cut

/** What a proposer of amendments to the coder's standing text reads: one task's blind brief, the
  * standing text its runs began with, the runs as the bench kept them, and every amendment
  * proposed before; and the text it is said, which points at them. Each path is a file's under
  * [[EvidenceCheckout.Evidence]], as [[EvidenceCheckout.write]] places a shown file.
  */
object Proposal {

  /** The file a proposer is said: `PROPOSE.md`. */
  val Said: RelPath = path("PROPOSE.md")

  /** The most characters of one tool result a run's file shows: [[Cut.Default]]'s. */
  val ResultChars: Int = Cut.Default match {
    case Cut.Within(chars) => chars
    case Cut.Whole => Int.MaxValue
  }

  /** The version of its section a standing text is the text of, which an amendment's Diff names
    * as the version it is against.
    */
  opaque type Against = Long

  object Against {

    /** `raw` as a version, or why not, naming it as `--against`: it is not a whole number of 1 or
      * more.
      */
    def of(raw: String): Either[String, Against] =
      raw.toLongOption
        .filter(_ >= 1)
        .toRight(s"--against $raw: not a version, a whole number of 1 or more")

    def version(a: Against): Long = a
  }

  /** A run a proposer reads: its session, and its transcript as the bench kept it. */
  final case class Run(session: SessionId, kept: Bench.Transcript) extends caps.Pure

  /** The trials of `kept` whose sessions' local ids are `named`, in `named`'s order: grit's,
    * each begun with `standing`, or kept before the bench recorded the standing text a trial began
    * with. Or why not, naming the id: none is named, or one is named twice, is no trial's or more
    * than one's (of two harnesses), is not grit's, or began with another standing text; or the
    * trials are of more than one task, naming each.
    */
  def chosen(
      kept: Vector[TrialRows.Trial],
      named: Vector[String],
      standing: Standing
  ): Either[String, Vector[TrialRows.Trial]] = {
    val picked = named.zipWithIndex.foldLeft[Either[String, Vector[TrialRows.Trial]]](
      Either.cond(named.nonEmpty, Vector.empty, "no trial is named")
    ) { case (done, (local, i)) =>
      done.flatMap { got =>
        kept.filter(_.session.local == local) match {
          case _ if named.indexOf(local) < i => Left(s"trial $local is named twice")
          case Vector(t) if t.session.harness != Harness.Grit =>
            Left(s"trial $local is not grit's, so it began with no standing text")
          case Vector(t) if t.standing.exists(_ != standing.sha256) =>
            Left(s"trial $local began with another standing text than the one given")
          case Vector(t) => Right(got :+ t)
          case Vector() => Left(s"trial $local is no trial the bench keeps")
          case _ => Left(s"trial $local names more than one trial, of different harnesses")
        }
      }
    }
    picked.flatMap { trials =>
      val tasks = trials.map(_.task).distinct
      Either.cond(
        tasks.size <= 1,
        trials,
        s"the trials are of more than one task: ${tasks.map(TrialRows.TaskName.value).mkString(", ")}"
      )
    }
  }

  /** The path of the `n`th of the runs [[of]] is given, counted from 1. */
  def trajectory(n: Int): RelPath = path(s"trajectories/$n.md")

  /** The path of the `n`th of the amendments [[of]] is given, counted from 1. */
  def amendment(n: Int): RelPath = path(s"amendments/$n.md")

  /** `task`'s proposal: [[Said]], `brief` then the list of what follows, each by its path in the
    * checkout, `standing.md`'s naming `against`, its section's version; `task.md`, the task's
    * brief; `standing.md`, `standing`; each of `runs` at
    * `trajectories/<n>.md`, numbered from 1 in order, a line naming its session first (and saying
    * so when the bench clipped it), then its transcript with each part a turn showed before marked
    * ([[Trajectory.deduplicated]]) and each tool result cut to [[ResultChars]]
    * ([[Trajectory.within]]); and each of `amendments`, an amendment's text as
    * exported, at [[amendment]]. Or each file holding a grade shape ([[Shown.of]]), an amendment
    * among them.
    */
  def of(
      task: BlindTask,
      runs: Vector[Run],
      standing: Standing,
      against: Against,
      amendments: Vector[String],
      brief: String
  ): Either[Vector[GradeFound], Shown] = {
    val trajectories = runs.zipWithIndex.map { (run, i) =>
      val named = s"Run ${i + 1}, session ${run.session.local}." +
        (if (run.kept.clipped) s" $Clipped" else "")
      trajectory(i + 1) ->
        s"$named\n\n${Trajectory.within(Trajectory.deduplicated(run.kept.text), ResultChars)}"
    }
    val earlier = amendments.zipWithIndex.map((text, i) => amendment(i + 1) -> text)
    val taskFile = path("task.md")
    val standingFile = path("standing.md")
    def at(p: RelPath) = s"`${EvidenceCheckout.Evidence}/${RelPath.value(p)}`"
    val listed = Vector(
      s"- `${EvidenceCheckout.Base}/`: the repository the coder started from, at the commit the " +
        "task names.",
      s"- ${at(taskFile)}: the task, as its coder was given it.",
      s"- ${at(standingFile)}: the standing text every run began with, its section's text at " +
        s"version ${Against.version(against)}, the version an amendment's Diff is against."
    ) ++ trajectories.map((p, _) =>
      s"- ${at(p)}: one run of the task, each model call in order. A tool result longer than " +
        f"$ResultChars%,d characters is cut there, with a line saying how much was left out."
    ) ++ (
      if (earlier.isEmpty) Vector("- No amendment has been proposed before.")
      else
        earlier.map((p, _) => s"- ${at(p)}: an amendment proposed before, with what became of it.")
    )
    Shown.of(
      Vector(
        Said -> (brief.trim + "\n\n## The evidence\n\nEach path is under the directory you work " +
          "in.\n\n" + listed.mkString("", "\n", "\n")),
        taskFile -> (
          s"# The task: ${TrialRows.TaskName.value(task.name)}\n\n" +
            s"Its coder started from commit ${Sha.value(task.base)}. Its brief, as its coder " +
            s"was given it:\n\n${task.brief}"
        ),
        standingFile -> standing.text
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
