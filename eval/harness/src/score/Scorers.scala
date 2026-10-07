package grit.eval.harness.score

import grit.core.triage.Kind
import grit.eval.harness.capture.Case

/** A case answered and labelled on one tag: its averaged probability of yes, and its label. */
final case class Judged(c: Case, p: Double, yes: Boolean)

/** Brier scores: a probability's squared distance from what was labelled, 0 at best. */
object Brier {

  /** Each case's (p − y)², y 1 for yes. */
  def tag(judged: Vector[Judged]): Vector[(Case, Double)] =
    judged.map(j => j.c -> square(j.p - (if (j.yes) 1.0 else 0.0)))

  /** Each case's Σ over kinds of (p − y)², y 1 for the kind labelled; a kind `ps` lacks is
    * read as 0.
    */
  def kind(judged: Vector[(Case, Map[Kind, Double], Kind)]): Vector[(Case, Double)] =
    judged.map((c, ps, labelled) =>
      c -> Kind.values.toVector
        .map(k => square(ps.getOrElse(k, 0.0) - (if (k == labelled) 1.0 else 0.0)))
        .sum
    )

  /** Each case's Σ over its placing's probabilities of (p − y)², y 1 at position `at` (the
    * exchange labelled, or beginning anew, the last).
    */
  def place(judged: Vector[(Case, Placing, Int)]): Vector[(Case, Double)] =
    judged.map((c, placing, at) =>
      c -> placing.ps.zipWithIndex.map((p, i) => square(p - (if (i == at) 1.0 else 0.0))).sum
    )

  /** Each case's score on `q`: [[tag]], [[kind]] or [[place]] over what `s` pairs. */
  def of(q: Target, s: Scoring): Vector[(Case, Double)] = q match {
    case Target.Tagged(t) => tag(s.tag(t))
    case Target.Kinds => kind(s.kind)
    case Target.Places => place(s.place)
  }

  private def square(x: Double): Double = x * x
}

/** One of five equal-width bins of predicted probability, `from` up to `to` (the last taking 1
  * too): how many cases fell in it, their mean prediction (`None` when none did), and the
  * proportion of them labelled yes.
  */
final case class Bin(
    from: Double,
    to: Double,
    n: Int,
    predicted: Option[Double],
    rate: Proportions
)

object Reliability {

  /** The five bins of `judged`, lowest first. */
  def of(judged: Vector[Judged]): Vector[Bin] =
    Vector.tabulate(5) { i =>
      val (from, to) = (i / 5.0, (i + 1) / 5.0)
      val in = judged.filter(j => j.p >= from && (j.p < to || (i == 4 && j.p <= to)))
      Bin(
        from,
        to,
        in.size,
        Option.when(in.nonEmpty)(in.map(_.p).sum / in.size),
        Proportions.of(in.map(j => j.c -> j.yes))
      )
    }
}

/** Deciding yes at `threshold`, when p is at or above it: the true-positive rate, over the cases
  * labelled yes, and the true-negative rate, over those labelled no; `shipped` when it is the
  * threshold grit decides at.
  */
final case class Point(threshold: Double, tpr: Proportions, tnr: Proportions, shipped: Boolean)

object Sweep {

  /** 0.05 to 0.95 by 0.05. */
  val Thresholds: Vector[Double] = Vector.tabulate(19)(k => (k + 1) / 20.0)

  /** `judged` decided at each of [[Thresholds]], `shipped` (`None`: no threshold) marked. */
  def of(judged: Vector[Judged], shipped: Option[Double]): Vector[Point] = {
    val (yes, no) = judged.partition(_.yes)
    Thresholds.map(t =>
      Point(
        t,
        Proportions.of(yes.map(j => j.c -> (j.p >= t))),
        Proportions.of(no.map(j => j.c -> (j.p < t))),
        shipped.exists(s => math.abs(s - t) < 1e-9)
      )
    )
  }
}

/** At cost ratio `ratio`, a false negative's cost over a false positive's: the threshold whose
  * mean cost per case is least (`best`), the cost there and at the shipped threshold, in false
  * positives' cost per case, and whether the shipped threshold is within error of the best: the
  * 95% interval of their paired difference, by exchange, reaches 0 (`None` under two
  * clusters).
  */
final case class Costed(
    ratio: Double,
    best: Double,
    atBest: Clustered,
    atShipped: Clustered,
    within: Option[Boolean]
)

object Cost {

  /** 1/16 to 64, by doubling: a ratio is an experiment's parameter, so the curve spans it. */
  val Ratios: Vector[Double] = Vector.tabulate(11)(j => math.pow(2, j - 4))

  /** For each of [[Ratios]], `judged` decided at the cheapest of [[Sweep.Thresholds]] and
    * `shipped` (the lowest, on a tie) and at `shipped`; empty when `judged` is.
    */
  def curve(judged: Vector[Judged], shipped: Double): Vector[Costed] =
    if (judged.isEmpty) Vector.empty
    else
      Ratios.map { r =>
        def costs(t: Double): Vector[(Case, Double)] = judged.map(j =>
          j.c -> (if (j.yes) { if (j.p >= t) 0.0 else r }
                  else { if (j.p >= t) 1.0 else 0.0 })
        )
        val best = (Sweep.Thresholds :+ shipped).sorted.maxBy(t => -mean(costs(t)))
        val (b, s) = (costs(best), costs(shipped))
        val diff = Clustered.of(b.zip(s).map((x, y) => x._1 -> (y._2 - x._2)))
        Costed(
          r,
          best,
          Clustered.of(b),
          Clustered.of(s),
          diff.exchange.map(_.interval._1 <= 0)
        )
      }

  private def mean(v: Vector[(Case, Double)]): Double = v.map(_._2).sum / v.size
}
