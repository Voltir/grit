package grit.eval.harness.score

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{QuestionName, ShadowName}
import grit.core.period.Probability
import grit.core.review.{Reason, Verdict}
import grit.eval.harness.corpus.CaseId
import grit.eval.harness.label.Verdicts
import grit.eval.harness.log.{Row, Weights}
import grit.lifecycle.triage.TriageQuestions

/** Verdicts against a live log and a question set's: each shadow's and reason's [[Judgement.Of]],
  * the shadows by name and the reasons in `Reason`'s order within one; and `undecided`, the
  * verdicts on a case either gate could not decide (a log has no answered row of it, or a gate
  * cannot read its row).
  */
final case class Judgement(reasons: Vector[Judgement.Of], undecided: Int)

object Judgement {

  /** `matched` of `of`. */
  final case class Matched(matched: Int, of: Int)

  /** The verdicts on the messages picked against `shadow` for `reason`, over the cases both
    * gates decided: `n` of them; how many live's helps gate (`live`) and the set's draft (`set`)
    * each decided as the verdict's [[Verdict.speaks]]; and `to`, of those whose `to` the set
    * answered, how many it read as the verdict's [[Verdict.toPerson]] (`None` when the set names
    * no `to`).
    */
  final case class Of(
      shadow: ShadowName,
      reason: Reason,
      n: Int,
      live: Int,
      set: Int,
      to: Option[Matched]
  )

  /** The set's `to` at least this reads as meant for a person. */
  val ToAt: Probability = Probability.clamped(0.5)

  /** `verdicts` against `live`'s rows and `set`'s, each case's first answered triage row of
    * each: live's helps gate as [[Drafts.of]] reads it at `helpsAt`, the set's draft as `gate`
    * reads its answers, and `to`, when given, the set's yes/no question at [[ToAt]]. Counts
    * only, by the reason each message was picked: nothing is weighted back by how often a
    * reason is picked.
    */
  def of(
      live: Vector[Row[Vector[Weights]]],
      set: Vector[Row[VectorMap[QuestionName, Answer]]],
      gate: TriageQuestions.Gate,
      helpsAt: Probability,
      to: Option[QuestionName],
      verdicts: Verdicts
  ): Judgement = {
    val lives: Map[CaseId, Vector[Weights]] =
      Drafts.firstAnswered(live).map(x => x._1 -> x._3).toMap
    val sets: Map[CaseId, VectorMap[QuestionName, Answer]] =
      Drafts.firstAnswered(set).map(x => x._1 -> x._3).toMap
    val decided = verdicts.cases.toVector.flatMap { (c, rated) =>
      for {
        ws <- lives.get(c)
        answers <- sets.get(c)
        a <- Drafts.helpsGate(ws, helpsAt)
        b <- gate.drafts(answers)
      } yield (rated, a, b, to.flatMap(yes(answers, _)))
    }
    val groups = decided
      .groupBy((r, _, _, _) => (r.shadow, r.reason))
      .toVector
      .sortBy { case ((shadow, reason), _) => (ShadowName.value(shadow), reason.ordinal) }
    Judgement(
      groups.map { case ((shadow, reason), cases) =>
        def matched(f: (Verdict, Boolean, Boolean, Option[Boolean]) => Boolean) =
          cases.count((r, a, b, t) => f(r.verdict, a, b, t))
        Of(
          shadow,
          reason,
          cases.size,
          matched((v, a, _, _) => a == v.speaks),
          matched((v, _, b, _) => b == v.speaks),
          to.map(_ =>
            Matched(
              matched((v, _, _, t) => t.contains(v.toPerson)),
              cases.count(_._4.isDefined)
            )
          )
        )
      },
      verdicts.cases.size - decided.size
    )
  }

  /** `q`'s yes/no in `answers` at [[ToAt]]; `None` when it is not a yes/no there. */
  private def yes(answers: VectorMap[QuestionName, Answer], q: QuestionName): Option[Boolean] =
    answers.get(q).collect { case Answer.YesNo(p) => Probability.clamped(p) >= ToAt }
}
