package grit.eval.harness.score

import grit.core.triage.Kind
import grit.eval.harness.capture.CaseId
import grit.eval.harness.log.{Outcome, Row, Suite, Weights}

/** Jev's own spread on each of triage's questions: the pooled standard deviation of a run's
  * repeats of one request, of the probability of the case's likeliest kind (`kind`) and of yes
  * to each tag.
  */
final case class Noise(kind: Double, waiting: Double, durable: Double, helps: Double) {

  /** The spread on tag `t`. */
  def of(t: Tag): Double = t match {
    case Tag.Waiting => waiting
    case Tag.Durable => durable
    case Tag.Helps => helps
  }
}

/** The cases whose state moved on between live triage and its replica shadow (the shipped
  * wording, of the live model), found under `noise`: those whose replica answer stands from
  * live's, on some question, by more than [[MovedOn.Multiple]] times `noise` on it.
  */
final case class Moved(noise: Noise, cases: Vector[CaseId])

object MovedOn {

  /** How many of Jev's own standard deviations a replica's answer may stand from live's before
    * the case is taken to have moved on: 10.
    */
  val Multiple: Double = 10.0

  /** Jev's spread in `rows`, over the cases whose triage was answered at least twice; `None`
    * when none was.
    */
  def noise(rows: Vector[Row[Vector[Weights]]]): Option[Noise] = {
    val answered: Vector[(CaseId, Vector[Double])] = rows.collect {
      case Row(
            Suite.Triage,
            id,
            _,
            _,
            _,
            _,
            _,
            Outcome.Answered(
              Vector(Weights.Choice(_, ps, _), Weights.YesNo(w), Weights.YesNo(d), Weights.YesNo(h))
            ),
            _,
            _,
            _,
            _
          ) if ps.size == Kind.values.size =>
        id -> (Answers.normalised(ps) ++ Vector(w, d, h))
    }
    // Per case answered twice or more: its likeliest kind's and each tag's values.
    val repeated: Vector[Vector[Vector[Double]]] =
      answered.groupMap(_._1)(_._2).values.toVector.filter(_.size >= 2).map {
        (vs: Vector[Vector[Double]]) =>
          val kinds = Kind.values.size
          val means = (0 until kinds).map(i => vs.map(_.lift(i).getOrElse(0.0)).sum)
          val likeliest = means.indices.maxByOption(means).getOrElse(0)
          vs.map(v => (likeliest +: (kinds until kinds + 3)).map(v.lift(_).getOrElse(0.0)).toVector)
      }
    Option.when(repeated.nonEmpty) {
      def pooled(i: Int): Double = {
        val columns = repeated.map(_.map(_.lift(i).getOrElse(0.0)))
        val squares = columns.map { (c: Vector[Double]) =>
          val mean = c.sum / c.size
          c.map(x => (x - mean) * (x - mean)).sum
        }.sum
        math.sqrt(squares / columns.map(_.size - 1).sum)
      }
      Noise(pooled(0), pooled(1), pooled(2), pooled(3))
    }
  }

  /** The cases both `replica` and `live` answered that moved on under `noise` ([[Moved]]):
    * compared on each tag's probability of yes, and on the probability each gives live's
    * likeliest kind. Ids in their written order.
    */
  def of(replica: Answers, live: Answers, noise: Noise): Moved =
    Moved(
      noise,
      live.triage.toVector
        .flatMap((id, l) =>
          replica.triage.get(id).map { r =>
            val kind = l.mean.likeliest
            val kindGap = math.abs(
              r.mean.kinds.getOrElse(kind, 0.0) - l.mean.kinds.getOrElse(kind, 0.0)
            ) > Multiple * noise.kind
            val tagGap = Tag.values
              .exists(t => math.abs(Tag.of(t, r.mean) - Tag.of(t, l.mean)) > Multiple * noise.of(t))
            id -> (kindGap || tagGap)
          }
        )
        .collect { case (id, true) => id }
        .sorted
    )
}
