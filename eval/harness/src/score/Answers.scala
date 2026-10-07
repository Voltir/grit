package grit.eval.harness.score

import grit.core.period.Probability
import grit.core.triage.{Earning, Kind}
import grit.eval.harness.capture.CaseId
import grit.eval.harness.label.Labelled
import grit.eval.harness.log.{Outcome, Row, Suite, Weights}

/** One of triage's yes/no questions. */
enum Tag {
  case Waiting, Durable, Helps
}

object Tag {

  /** `t`'s written name: `waiting`, `durable` or `helps`. */
  def written(t: Tag): String = t.toString.toLowerCase

  /** The tag written `name`; `None` for no tag's. */
  def read(name: String): Option[Tag] = values.find(written(_) == name)

  /** The probability of yes `a` gives `t`. */
  def of(t: Tag, a: Triage): Double = t match {
    case Waiting => a.waiting
    case Durable => a.durable
    case Helps => a.helps
  }

  /** What `l` labels `t`, yes as `true`. */
  def labelled(t: Tag, l: Labelled): Option[Boolean] = t match {
    case Waiting => l.waiting
    case Durable => l.durable
    case Helps => l.helps
  }

  /** The threshold `t` is scored at, yes at or above it: `durable` at [[Earning.DurableAt]],
    * where grit decides it; `helps` at 0.60, the `helps` a deployment's speech started on while
    * its gate was triage's tags alone; `None` for `waiting`, on which grit decides nothing by threshold.
    */
  def shipped(t: Tag): Option[Double] = t match {
    case Waiting => None
    case Durable => Some(Probability.value(Earning.DurableAt))
    case Helps => Some(0.60)
  }
}

/** A case's triage answers: each kind's probability (summing to 1 unless every kind was given
  * 0), and each tag's probability of yes.
  */
final case class Triage(kinds: Map[Kind, Double], waiting: Double, durable: Double, helps: Double) {

  /** The kind given the most (on a tie, the first in [[Kind]]'s order). */
  def likeliest: Kind =
    Kind.values.foldLeft(Kind.Question)((best, k) =>
      if (kinds.getOrElse(k, 0.0) > kinds.getOrElse(best, 0.0)) k else best
    )
}

/** A question a run answers for a case: one of triage's tags, triage's kind, or stitching's
  * place.
  */
enum Target {
  case Tagged(tag: Tag)
  case Kinds
  case Places
}

object Target {

  /** Every question, in the order reports list them. */
  val all: Vector[Target] = Kinds +: Tag.values.toVector.map(Tagged(_)) :+ Places

  /** `t`'s written name: `kind`, a tag's ([[Tag.written]]), or `place`. */
  def written(t: Target): String = t match {
    case Tagged(tag) => Tag.written(tag)
    case Kinds => "kind"
    case Places => "place"
  }

  /** The question written `name`; `None` for no question's. */
  def read(name: String): Option[Target] = all.find(written(_) == name)

  /** Whether `l` labels `t`. */
  def labelled(t: Target, l: Labelled): Boolean = t match {
    case Tagged(tag) => Tag.labelled(tag, l).isDefined
    case Kinds => l.kind.isDefined
    case Places => l.place.isDefined
  }
}

/** A case's stitch answer: each offered exchange's probability, in the order they were offered,
  * then beginning anew's (summing to 1 unless every one was given 0).
  */
final case class Placing(ps: Vector[Double])

/** One case's answer in a run, its answered repeats averaged (`mean`), and how far apart they
  * were: each probability's max − min across them (`spread`; 0 with one repeat).
  */
final case class Averaged[A](mean: A, spread: A, repeats: Int)

/** A run's answers, by case, each averaged over its repeats. */
final case class Answers(
    triage: Map[CaseId, Averaged[Triage]],
    stitch: Map[CaseId, Averaged[Placing]]
)

object Answers {

  /** The answered rows of `rows`, by suite and case, averaged over their repeats, a choice's
    * probabilities first normalised to sum to 1 (a negative or infinite one read as 0). A
    * triage row counts when it holds a choice of every kind and three yes/no answers; a stitch
    * row when it holds one choice of as many probabilities as the case's first such row. Other
    * rows, and rows not answered, are left out; a case none of whose rows count is absent.
    */
  def of(rows: Vector[Row[Vector[Weights]]]): Answers = {
    val answered: Vector[(Suite, CaseId, Vector[Weights])] =
      rows.collect { case Row(suite, id, _, _, _, _, _, Outcome.Answered(ws), _, _, _, _) =>
        (suite, id, ws)
      }
    val triage: Vector[(CaseId, Vector[Double])] = answered.collect {
      case (
            Suite.Triage,
            id,
            Vector(Weights.Choice(_, ps, _), Weights.YesNo(w), Weights.YesNo(d), Weights.YesNo(h))
          ) if ps.size == Kind.values.size =>
        id -> (normalised(ps) ++ Vector(w, d, h))
    }
    val stitch: Vector[(CaseId, Vector[Double])] = answered.collect {
      case (Suite.Stitch, id, Vector(Weights.Choice(_, ps, _))) =>
        id -> normalised(ps)
    }
    def triageOf(v: Vector[Double]): Triage =
      Triage(Kind.values.toVector.zip(v).toMap, at(v, 5), at(v, 6), at(v, 7))
    Answers(
      averaged(triage).map((id: CaseId, a: Columns) =>
        id -> Averaged(triageOf(a.mean), triageOf(a.spread), a.n)
      ),
      averaged(stitch).map((id: CaseId, a: Columns) =>
        id -> Averaged(Placing(a.mean), Placing(a.spread), a.n)
      )
    )
  }

  /** `ps` scaled to sum to 1, a negative or infinite one read as 0; unscaled when they sum to
    * 0.
    */
  private[score] def normalised(ps: Vector[Double]): Vector[Double] = {
    val clean = ps.map(x => if (x >= 0 && !x.isInfinite) x else 0.0)
    val total = clean.sum
    if (total > 0) clean.map(_ / total) else clean
  }

  /** Per case, its vectors of the first one's length: their mean, each place's max − min, and
    * how many.
    */
  private def averaged(each: Vector[(CaseId, Vector[Double])]): Map[CaseId, Columns] =
    each.groupMap(_._1)(_._2).flatMap { (id: CaseId, vs: Vector[Vector[Double]]) =>
      vs.headOption.map { (first: Vector[Double]) =>
        val same = vs.filter(_.size == first.size)
        val columns: Vector[Vector[Double]] = same.transpose
        id -> Columns(
          columns.map((c: Vector[Double]) => c.sum / same.size),
          columns.map((c: Vector[Double]) => c.max - c.min),
          same.size
        )
      }
    }

  /** Vectors' mean and spread, place by place, and how many there were. */
  private final case class Columns(mean: Vector[Double], spread: Vector[Double], n: Int)

  private def at(v: Vector[Double], i: Int): Double = v.lift(i).getOrElse(0.0)
}
