package grit.eval.harness.score

import grit.core.triage.Kind
import grit.eval.harness.capture.Case
import grit.eval.harness.label.{Labels, Place}

/** A run's answers beside its capture's cases and their labels: what each scorer is given. */
final case class Scoring(cases: Vector[Case], answers: Answers, labels: Labels) {

  /** Each case answered and labelled `t`, in the capture's order. */
  def tag(t: Tag): Vector[Judged] =
    cases.flatMap(c =>
      for {
        a <- answers.triage.get(c.id)
        yes <- Tag.labelled(t, labels.of(c.id))
      } yield Judged(c, Tag.of(t, a.mean), yes)
    )

  /** Each case answered and labelled with a kind. */
  def kind: Vector[(Case, Map[Kind, Double], Kind)] =
    cases.flatMap(c =>
      for {
        a <- answers.triage.get(c.id)
        k <- labels.of(c.id).kind
      } yield (c, a.mean.kinds, k)
    )

  /** Each case answered and labelled with a place, and that place's position among the
    * exchanges its placing offers (beginning anew the last). A case whose placing does not
    * offer as many exchanges as the capture rebuilt for it, or does not offer the exchange
    * labelled, is left out: [[unplaced]] counts them.
    */
  def place: Vector[(Case, Placing, Int)] = placed.collect { case (c, p, Some(at)) => (c, p, at) }

  /** How many cases answered and labelled with a place [[place]] leaves out. */
  def unplaced: Int = placed.count(_._3.isEmpty)

  private def placed: Vector[(Case, Placing, Option[Int])] =
    cases.flatMap(c =>
      for {
        a <- answers.stitch.get(c.id)
        label <- labels.of(c.id).place
        slots <- c.stitch.map(_.rebuilt.map(_.root))
      } yield (
        c,
        a.mean,
        Option
          .when(a.mean.ps.size == slots.size + 1)(label match {
            case Place.Begins => Some(slots.size)
            case Place.Follows(root) => Option(slots.indexOf(root)).filter(_ >= 0)
          })
          .flatten
      )
    )
}
