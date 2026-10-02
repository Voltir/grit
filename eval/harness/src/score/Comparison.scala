package grit.eval.harness.score

import grit.core.triage.Kind
import grit.eval.harness.corpus.CaseId
import grit.eval.harness.label.Labels

/** The cases two runs both answered, by what became of each on one question from run A to run
  * B: `fixed` (A's decision not what was labelled, B's what was), `broken` (the reverse),
  * `moved` (decided differently, but neither: unlabelled, or a kind wrong both times), and how
  * many were decided the same; ids in their written order.
  */
final case class Changes(
    fixed: Vector[CaseId],
    broken: Vector[CaseId],
    moved: Vector[CaseId],
    unchanged: Int
)

object Comparison {

  /** `t` decided yes at or above `threshold` in each run. */
  def tag(t: Tag, threshold: Double, a: Answers, b: Answers, labels: Labels): Changes =
    changes(
      a.triage.flatMap((id, x) =>
        b.triage
          .get(id)
          .map(y =>
            (
              id,
              Tag.of(t, x.mean) >= threshold,
              Tag.of(t, y.mean) >= threshold,
              Tag.labelled(t, labels.of(id))
            )
          )
      )
    )

  /** The kind decided as each run's likeliest (on a tie, the first in [[Kind]]'s order). */
  def kind(a: Answers, b: Answers, labels: Labels): Changes =
    changes(
      a.triage.flatMap((id, x) =>
        b.triage.get(id).map(y => (id, likeliest(x.mean), likeliest(y.mean), labels.of(id).kind))
      )
    )

  private def likeliest(t: Triage): Kind =
    Kind.values.foldLeft(Kind.Question)((best, k) =>
      if (t.kinds.getOrElse(k, 0.0) > t.kinds.getOrElse(best, 0.0)) k else best
    )

  private def changes[D](decided: Iterable[(CaseId, D, D, Option[D])]): Changes = {
    val became = decided.toVector.sortBy(_._1).map { (id, was, is, label) =>
      id -> (
        if (was == is) None
        else if (label.contains(is)) Some(Became.Fixed)
        else if (label.contains(was)) Some(Became.Broken)
        else Some(Became.Moved)
      )
    }
    def of(b: Became) = became.collect { case (id, Some(`b`)) => id }
    Changes(of(Became.Fixed), of(Became.Broken), of(Became.Moved), became.count(_._2.isEmpty))
  }

  private enum Became {
    case Fixed, Broken, Moved
  }
}
