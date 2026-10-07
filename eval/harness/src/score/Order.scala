package grit.eval.harness.score

import grit.eval.harness.capture.{Case, CaseId}
import grit.eval.harness.label.Labels

/** Cases in the order a person should label them: the ones a label would settle most first,
  * those not yet labelled on the question before those that are, ties by id.
  */
object Order {

  /** The cases both runs answered on `q`, by how far apart they answered it, largest first: a
    * tag's probability of yes, or the largest change among a kind's or a placing's
    * probabilities (a case whose placings offer different exchanges is left out).
    */
  def difference(q: Target, a: Answers, b: Answers, labels: Labels): Vector[CaseId] =
    ordered(
      q,
      labels,
      q match {
        case Target.Tagged(t) =>
          a.triage.toVector.flatMap((id, x) =>
            b.triage.get(id).map(y => id -> math.abs(Tag.of(t, y.mean) - Tag.of(t, x.mean)))
          )
        case Target.Kinds =>
          a.triage.toVector.flatMap((id, x) =>
            b.triage
              .get(id)
              .map(y =>
                id -> x.mean.kinds.keySet
                  .union(y.mean.kinds.keySet)
                  .map(k =>
                    math.abs(y.mean.kinds.getOrElse(k, 0.0) - x.mean.kinds.getOrElse(k, 0.0))
                  )
                  .maxOption
                  .getOrElse(0.0)
              )
          )
        case Target.Places =>
          a.stitch.toVector.flatMap((id, x) =>
            b.stitch
              .get(id)
              .filter(_.mean.ps.size == x.mean.ps.size)
              .map(y =>
                id -> y.mean.ps
                  .zip(x.mean.ps)
                  .map((p, r) => math.abs(p - r))
                  .maxOption
                  .getOrElse(0.0)
              )
          )
      }
    )

  /** The cases of `cases` answered at least twice on `q` in `a`, by their repeats' spread on it
    * ([[Repeated]]), largest first: where the classifier is least sure of itself.
    */
  def spread(q: Target, cases: Vector[Case], a: Answers, labels: Labels): Vector[CaseId] =
    ordered(q, labels, Repeated.spread(q, cases, a).map((c, v) => c.id -> v))

  private def ordered(q: Target, labels: Labels, by: Vector[(CaseId, Double)]): Vector[CaseId] =
    by.sortBy((id, v) => (Target.labelled(q, labels.of(id)), -v, id.written)).map(_._1)
}
