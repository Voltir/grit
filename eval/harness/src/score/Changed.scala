package grit.eval.harness.score

import grit.core.message.Tokens
import grit.core.store.Focus
import grit.eval.harness.capture.{Case, CaseId, Digest}
import grit.eval.harness.log.{Outcome, Row, Suite, Weights}
import grit.eval.harness.stats.Estimate

/** The cases whose triage question two runs asked differently: what a change of input, such as
  * a recipe, reached.
  */
object Changed {

  /** The cases of `cases` both `a` and `b` have triage rows for, whose rows' request digests
    * differ between them, in `cases`' order.
    */
  def of[A](cases: Vector[Case], a: Vector[Row[A]], b: Vector[Row[A]]): Vector[Case] = {
    def asked(rows: Vector[Row[A]]): Map[CaseId, Set[Digest]] =
      rows.filter(_.suite == Suite.Triage).groupMapReduce(_.id)(r => Set(r.request))(_ ++ _)
    val (x, y) = (asked(a), asked(b))
    cases.filter(c => x.get(c.id).zip(y.get(c.id)).exists(_ != _))
  }

  /** The focus each case's triage rows in `rows` record ([[Row.focus]]); a case whose rows
    * record none, or disagree, is absent.
    */
  def focus[A](rows: Vector[Row[A]]): Map[CaseId, Focus] =
    rows
      .filter(_.suite == Suite.Triage)
      .groupMap(_.id)(_.focus)
      .toVector
      .collect { case (id, fs) if fs.distinct.size == 1 => fs.headOption.flatten.map(id -> _) }
      .flatten
      .toMap
}

/** A measure per call: its mean, and its 90th percentile by nearest rank. */
final case class PerCall(mean: Double, p90: Double)

/** How large a run's answered triage calls were: how many (`calls`), their input tokens (a
  * cached row's as first asked), and their cost in USD over those that reported one (`None`
  * when none did; `input` `None` when none was answered).
  */
final case class Size(calls: Int, input: Option[PerCall], cost: Option[PerCall])

object Size {

  def of[A](rows: Vector[Row[A]]): Size = {
    val answered = rows.filter(r =>
      r.suite == Suite.Triage && (r.outcome match {
        case Outcome.Answered(_) => true
        case _ => false
      })
    )
    // Summed as decimals, so a mean of costs keeps their digits.
    def per(xs: Vector[BigDecimal]): Option[PerCall] = {
      val sorted = xs.sorted
      sorted
        .lift(math.max(0, math.ceil(0.9 * sorted.size).toInt - 1))
        .map(p90 => PerCall((sorted.sum / sorted.size).toDouble, p90.toDouble))
    }
    Size(
      answered.size,
      per(answered.map(r => BigDecimal(Tokens.value(r.usage.input)))),
      per(answered.flatMap(_.usage.costUsd))
    )
  }
}

/** What Jev's repeat noise alone reads as a paired difference. */
object Apart {

  /** `measure` of each case on the answers of `rows`' second repeat (repeat 1) less those of its
    * first (repeat 0), clustered: the difference a run compared with itself shows.
    */
  def of(
      rows: Vector[Row[Vector[Weights]]],
      measure: Answers => Vector[(Case, Double)]
  ): Clustered = {
    def at(repeat: Int) = measure(Answers.of(rows.filter(_.repeat == repeat)))
    val first = at(0).map((c, v) => c.id -> v).toMap
    Clustered.of(at(1).flatMap((c, v) => first.get(c.id).map(x => c -> (v - x))))
  }

  /** About the least difference a paired comparison of two runs of `repeats` repeats each
    * detects from Jev's noise alone, `apart` being one repeat less another ([[of]]): `apart`'s
    * MDE over √`repeats`, a mean of `repeats` answers varying √`repeats` times less than one.
    */
  def implied(apart: Estimate, repeats: Int): Double = apart.mde / math.sqrt(repeats)
}
