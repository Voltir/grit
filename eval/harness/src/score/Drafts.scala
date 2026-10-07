package grit.eval.harness.score

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.period.Probability
import grit.core.store.Focus
import grit.core.triage.{Earning, Gate}
import grit.eval.harness.capture.CaseId
import grit.eval.harness.log.{Outcome, Row, Suite}

/** The cases of two yes/no decisions over the same cases, by how each decided: yes by both,
  * by A only, by B only, by neither; each in the order the cases were given.
  */
final case class Cells(
    both: Vector[CaseId],
    aOnly: Vector[CaseId],
    bOnly: Vector[CaseId],
    neither: Vector[CaseId]
)

object Cells {
  val Empty: Cells = Cells(Vector.empty, Vector.empty, Vector.empty, Vector.empty)
}

/** One probability a question set answered, over the cases it answered: a yes/no's
  * probability of yes, named for its question, or a choice key's, named `<question>.<key>`;
  * its mean, its standard deviation (population, 0 for one case) and how many cases.
  */
final case class Column(name: String, mean: Double, sd: Double, n: Int)

/** One side of a draft comparison: a log's `rows`, each answered row's answers under their
  * names; the `gate` its draft is derived by; and its yes/no questions read as worth keeping
  * (`durable`, at [[Earning.DurableAt]]) and as meant for someone (`to`, at [[Judgement.ToAt]]),
  * each `None` when its set asks none.
  */
final case class Drafting(
    rows: Vector[Row[VectorMap[QuestionName, Answer]]],
    gate: Gate,
    durable: Option[QuestionName],
    to: Option[QuestionName]
)

/** Live triage's draft beside a question set's, over the cases both live's log and the set's
  * answered: `gate`, live's draft as A and the set's as B, by the focus each case was said at
  * (`None`: its rows record none); `undecided`, the cases both answered on which either gate
  * reads an answer its row lacks; `columns`, each of the set's probabilities in the order
  * first asked; and `durable`, live's durable question as A and the set's as B (`None` when
  * either asks none).
  */
final case class Drafts(
    gate: Map[Option[Focus], Cells],
    undecided: Int,
    columns: Vector[Column],
    durable: Option[Cells]
)

object Drafts {

  /** `set` against `live`, each case's first answered triage row of each: each side's draft
    * is its gate over its answers ([[Gate.drafts]]), and each side's durable its `durable`
    * question.
    */
  def of(live: Drafting, set: Drafting): Drafts = {
    val lives = firstAnswered(live.rows)
    val sets = firstAnswered(set.rows)
    val paired = sets.flatMap((c, setRow, answers) =>
      lives.collectFirst { case (`c`, liveRow, theirs) =>
        (c, liveRow.focus.orElse(setRow.focus), theirs, answers)
      }
    )
    val gated = paired.map((c, focus, theirs, answers) =>
      (c, focus, live.gate.drafts(theirs).zip(set.gate.drafts(answers)))
    )
    val decided = gated.collect { case (c, focus, Some((a, b))) => (c, focus, a, b) }
    Drafts(
      decided
        .map(_._2)
        .distinct
        .map(f => f -> cells(decided.collect { case (c, `f`, a, b) => (c, a, b) }))
        .toMap,
      gated.size - decided.size,
      columns(sets.map(_._3)),
      live.durable
        .zip(set.durable)
        .map((ql, qs) =>
          cells(
            paired.flatMap((c, _, theirs, answers) =>
              yes(theirs, ql).zip(yes(answers, qs)).map((a, b) => (c, a, b))
            )
          )
        )
    )
  }

  /** Each case's first answered triage row in `rows`, with its answer, in the order first
    * seen.
    */
  private[score] def firstAnswered[A](rows: Vector[Row[A]]): Vector[(CaseId, Row[A], A)] =
    rows
      .collect { case r @ Row(Suite.Triage, c, _, _, _, _, _, Outcome.Answered(a), _, _, _, _) =>
        (c, r, a)
      }
      .distinctBy(_._1)

  /** `q`'s yes/no in `answers` at [[Earning.DurableAt]]; `None` when it is not a yes/no there. */
  private def yes(answers: VectorMap[QuestionName, Answer], q: QuestionName): Option[Boolean] =
    answers.get(q).collect { case Answer.YesNo(p) => Probability.clamped(p) >= Earning.DurableAt }

  private def cells(cases: Vector[(CaseId, Boolean, Boolean)]): Cells =
    Cells(
      cases.collect { case (c, true, true) => c },
      cases.collect { case (c, true, false) => c },
      cases.collect { case (c, false, true) => c },
      cases.collect { case (c, false, false) => c }
    )

  /** Each probability of `answered`, in the order first asked, with its mean and spread. */
  private def columns(answered: Vector[VectorMap[QuestionName, Answer]]): Vector[Column] = {
    val each: Vector[Vector[(String, Double)]] = answered.map(_.toVector.flatMap { (q, a) =>
      val n = QuestionName.value(q)
      a match {
        case Answer.YesNo(p) => Vector(n -> p)
        case Answer.Choice(_, weights, _) => weights.map(w => s"$n.${w.key}" -> w.probability)
      }
    })
    val names = each.flatten.map(_._1).distinct
    names.map { n =>
      val ps = each.flatMap(_.collectFirst { case (`n`, p) => p })
      val mean = ps.sum / ps.size
      Column(n, mean, math.sqrt(ps.map(p => (p - mean) * (p - mean)).sum / ps.size), ps.size)
    }
  }
}
