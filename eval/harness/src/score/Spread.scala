package grit.eval.harness.score

import grit.core.period.Probability
import grit.core.triage.Kind
import grit.eval.harness.corpus.{Case, CaseId, Live, SeenCheck}
import grit.eval.harness.log.{Outcome, Row, Suite, Weights}

/** How far apart answers are that would be equal were the classifier deterministic: the
  * `largest` gap between two probabilities, the questions, by suite and case, where any gap is
  * over [[Spread.Tolerance]], and how many were compared.
  */
final case class Spread(largest: Double, spread: Vector[(Suite, CaseId)], compared: Int)

object Spread {

  /** The most two probabilities may differ by and count as equal. */
  val Tolerance = 1e-9

  /** Across repeats: for each suite and case, each probability's max − min over the rows that
    * answered it, a choice's probabilities normalised to sum to 1. Rows not answered are left
    * out; a question answered once is compared, with no gap.
    */
  def repeats(rows: Vector[Row[Vector[Weights]]]): Spread = {
    val answers: Vector[Answered] = answered(rows)
    of(answers.map(_.question).distinct.map { (q: Question) =>
      val same: Vector[Vector[Double]] = answers.filter(_.question == q).map(_.ps)
      val gaps: Vector[Double] = same.transpose.map((ps: Vector[Double]) => ps.max - ps.min)
      Gap(q, gaps.maxOption.getOrElse(0.0))
    })
  }

  /** Against what triage and stitching kept live (`cases`): each answered row's
    * probabilities, a choice's normalised to sum to 1, against its case's live tags (the live
    * kind's probability and the three yes/no) or live placement (each offered exchange's,
    * when its seen check matched). Rows whose case has no live answer to compare are left out.
    */
  def live(rows: Vector[Row[Vector[Weights]]], cases: Vector[Case]): Spread = {
    val byId = cases.map(c => c.id -> c).toMap
    def kept(r: Row[Vector[Weights]]): Option[Vector[Option[Double]]] =
      byId.get(r.id).flatMap { c =>
        r.suite match {
          case Suite.Triage =>
            c.tags match {
              case Live.Weighed(kind, kindP, w, d, h, _, _) =>
                val kinds = Vector.tabulate(Kind.values.size)(i =>
                  Option.when(i == kind.ordinal)(Probability.value(kindP))
                )
                Some(kinds ++ Vector(w, d, h).map(x => Some(Probability.value(x))))
              case Live.Named(_, _, _) | Live.Unanswered(_) => None
            }
          case Suite.Stitch =>
            c.stitch
              .filter(_.seen == SeenCheck.Match)
              .map(s => s.offered.map(_.p.map(Probability.value)) :+ None)
        }
      }
    val gaps: Vector[Gap] = answered(rows).flatMap { (a: Answered) =>
      kept(a.row).filter(_.size == a.ps.size).map { (live: Vector[Option[Double]]) =>
        val diffs = a.ps.zip(live).collect { case (x, Some(y)) => math.abs(x - y) }
        Gap(a.question, diffs.maxOption.getOrElse(0.0))
      }
    }
    of(gaps.map(_.question).distinct.map { (q: Question) =>
      Gap(q, gaps.filter(_.question == q).map(_.gap).maxOption.getOrElse(0.0))
    })
  }

  /** A question of a run: its suite and case. */
  private final case class Question(suite: Suite, id: CaseId)

  /** An answered row, its question, and its probabilities in order, a choice's normalised. */
  private final case class Answered(
      row: Row[Vector[Weights]],
      question: Question,
      ps: Vector[Double]
  )

  /** A question's largest gap. */
  private final case class Gap(question: Question, gap: Double)

  /** Each answered row with its probabilities in order, a choice's normalised. */
  private def answered(rows: Vector[Row[Vector[Weights]]]): Vector[Answered] =
    rows.collect { case r @ Row(_, _, _, _, _, _, _, Outcome.Answered(ws), _, _, _, _) =>
      Answered(
        r,
        Question(r.suite, r.id),
        ws.flatMap {
          case Weights.Choice(_, ps, _) => Answers.normalised(ps)
          case Weights.YesNo(yes) => Vector(yes)
        }
      )
    }

  /** The spread of each question's largest gap. */
  private def of(gaps: Vector[Gap]): Spread =
    Spread(
      gaps.map(_.gap).maxOption.getOrElse(0.0),
      gaps
        .filter(_.gap > Tolerance)
        .map(_.question)
        .sortBy(q => (q.suite.ordinal, q.id.written))
        .map(q => (q.suite, q.id)),
      gaps.size
    )
}
