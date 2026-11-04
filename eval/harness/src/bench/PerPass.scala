package grit.eval.harness.bench

import grit.core.id.Harness
import grit.core.message.Cost
import grit.core.model.ModelId
import grit.dbos.internal.TrialRows

/** Cents per passing trial: what a pass costs, the number the bench is improved against. Beside
  * it, quality is not scored yet.
  */
object PerPass {

  /** The trials of one `task`, `harness` and `model`: how many ran and `passed`, what they
    * `spent` together (failed ones included, as [[TrialRows.Trial.usage]] priced them: at least
    * that, when one was not priced), and the `distances` of the passing ones from the task's
    * known diff, in lines, oldest first (a failed trial's distance means nothing for a pass).
    */
  final case class Group(
      task: TrialRows.TaskName,
      harness: Harness,
      model: ModelId,
      trials: Int,
      passed: Int,
      spent: Cost,
      distances: Vector[Long]
  ) extends caps.Pure {

    /** What a pass cost: [[spent]] over [[passed]]; `None` with no pass. */
    def perPass: Option[Cost] =
      Option.when(passed > 0)(spent match {
        case Cost.Exact(usd) => Cost.Exact(usd / passed)
        case Cost.AtLeast(usd) => Cost.AtLeast(usd / passed)
      })
  }

  /** `trials` grouped by task, harness and model, in the order each group's first trial came. */
  def groups(trials: Vector[TrialRows.Trial]): Vector[Group] = {
    def key(t: TrialRows.Trial) = (t.task, t.session.harness, t.model)
    trials.map(key).distinct.map { k =>
      val of = trials.filter(key(_) == k)
      val passing = of.filter(_.passed)
      Group(
        k._1,
        k._2,
        k._3,
        of.size,
        passing.size,
        Cost.total(of.map(_.usage)),
        passing.flatMap(_.distance)
      )
    }
  }

  /** What a person reads of [[groups]] over `trials`: a column header line, then a line a group,
    * its columns aligned under the header.
    */
  def lines(trials: Vector[TrialRows.Trial]): Vector[String] = {
    val rows: Vector[Vector[String]] = groups(trials).map { g =>
      Vector(
        TrialRows.TaskName.value(g.task),
        Harness.value(g.harness),
        ModelId.value(g.model),
        g.trials.toString,
        g.passed.toString,
        g.perPass.fold("no pass")(_.inCents),
        g.spent.inCents,
        distance(g)
      )
    }
    val all: Vector[Vector[String]] = Header +: rows
    val widths =
      Header.indices.map(i => all.flatMap(_.lift(i)).map(_.length).maxOption.getOrElse(0))
    all.map(row => row.zip(widths).map((s, w) => s.padTo(w, ' ')).mkString("  ").stripTrailing())
  }

  /** The column names of [[lines]]' header. */
  private val Header: Vector[String] =
    Vector("task", "harness", "model", "trials", "passed", "¢/pass", "total", "distance")

  /** A group's distances from the task's known diff: one line count when they agree, else the
    * span they cover; `—` with no passing trial.
    */
  private def distance(g: Group): String =
    (g.distances.minOption, g.distances.maxOption) match {
      case (Some(lo), Some(hi)) if lo == hi => lo.toString
      case (Some(lo), Some(hi)) => s"$lo–$hi"
      case _ => "—"
    }
}
