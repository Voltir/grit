package grit.eval.harness.score

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.core.period.Probability
import grit.core.store.Focus
import grit.core.triage.{Earning, Gate, Kind}
import grit.eval.harness.corpus.CaseId
import grit.eval.harness.log.{Outcome, Row, Suite, Weights}

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

/** A question set's derived draft decision beside live triage's helps gate, over the cases
  * both a live log and the set's log answered: `gate`, live's helps gate as A and the set's
  * draft as B, by the focus each case was said at (`None`: its rows record none); `undecided`,
  * the cases both answered on which either decision could not be read; `columns`, each of the
  * set's probabilities in the order first asked; and `durable`, live's `durable` tag as A and
  * the set's durable question as B, each at [[Earning.DurableAt]] (`None` when the set names
  * none).
  */
final case class Drafts(
    gate: Map[Option[Focus], Cells],
    undecided: Int,
    columns: Vector[Column],
    durable: Option[Cells]
)

object Drafts {

  /** `set`'s rows against `live`'s, each case's first answered triage row of each: live's
    * helps gate is its `helps` at least `helpsAt` and its chosen kind not chatter, as live
    * speech gates them before its other checks; the set's draft is `gate` over its answers;
    * `durable`, when given, is the set's question compared with live's `durable`. A live row
    * reads when it holds a choice of every kind and three yes/no answers.
    */
  def of(
      live: Vector[Row[Vector[Weights]]],
      set: Vector[Row[VectorMap[QuestionName, Answer]]],
      gate: Gate,
      helpsAt: Probability,
      durable: Option[QuestionName]
  ): Drafts = {
    val lives = firstAnswered(live)
    val sets = firstAnswered(set)
    val paired = sets.flatMap((c, setRow, answers) =>
      lives.collectFirst { case (`c`, liveRow, ws) =>
        (c, liveRow.focus.orElse(setRow.focus), ws, answers)
      }
    )
    val gated = paired.map((c, focus, ws, answers) =>
      (c, focus, helpsGate(ws, helpsAt).zip(gate.drafts(answers)))
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
      durable.map(q =>
        cells(
          paired.flatMap((c, _, ws, answers) =>
            liveDurable(ws).zip(yes(answers, q)).map((a, b) => (c, a, b))
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

  /** Live's four answers' helps gate; `None` when they are not a choice of a kind and three
    * yes/no.
    */
  private[score] def helpsGate(ws: Vector[Weights], helpsAt: Probability): Option[Boolean] =
    ws match {
      case Vector(
            Weights.Choice(chosen, ps, _),
            Weights.YesNo(_),
            Weights.YesNo(_),
            Weights.YesNo(h)
          ) if ps.size == Kind.values.size =>
        Kind.values
          .lift(chosen)
          .map(kind => kind != Kind.Chatter && Probability.clamped(h) >= helpsAt)
      case _ => None
    }

  /** Live's `durable` at [[Earning.DurableAt]]; `None` as for [[helpsGate]]. */
  private def liveDurable(ws: Vector[Weights]): Option[Boolean] =
    ws match {
      case Vector(Weights.Choice(_, ps, _), Weights.YesNo(_), Weights.YesNo(d), Weights.YesNo(_))
          if ps.size == Kind.values.size =>
        Some(Probability.clamped(d) >= Earning.DurableAt)
      case _ => None
    }

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
