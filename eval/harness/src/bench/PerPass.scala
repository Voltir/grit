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

  /** What a person reads of [[groups]] over `trials`: a heading line, then a line a group. */
  def lines(trials: Vector[TrialRows.Trial]): Vector[String] = {
    val all = groups(trials)
    val names = all.map(g =>
      Vector(TrialRows.TaskName.value(g.task), Harness.value(g.harness), ModelId.value(g.model))
    )
    val widths = (0 until 3).map(i => names.flatMap(_.lift(i)).map(_.length).maxOption.getOrElse(0))
    val heading =
      "Cents per passing trial: every trial's cost by grit's rates, failed ones included, over " +
        "how many passed, by task, harness and model; distance (lines from the task's known " +
        "diff) over passing trials only; quality is not scored yet."
    heading +: all.zip(names).map { (g: Group, n: Vector[String]) =>
      val named = n.zip(widths).map((s, w) => s.padTo(w, ' ')).mkString("  ")
      val ran = s"${g.trials} ${if (g.trials == 1) "trial" else "trials"}, ${g.passed} passed"
      val inAll = if (g.trials == 1) "" else s" (${g.spent.inCents} in all)"
      val cost =
        g.perPass.fold(s"no pass (${g.spent.inCents} in all)")(p => s"${p.inCents} a pass$inAll")
      val distance = (g.distances.minOption, g.distances.maxOption) match {
        case (Some(lo), Some(hi)) if lo == hi => s"; distance $lo"
        case (Some(lo), Some(hi)) => s"; distance $lo–$hi"
        case _ => ""
      }
      s"$named  $ran: $cost$distance"
    }
  }
}
