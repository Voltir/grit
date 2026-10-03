package grit.eval.harness.score

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.{QuestionName, ShadowName}
import grit.core.period.Probability
import grit.core.review.{Reason, Verdict}
import grit.eval.harness.corpus.CaseId
import grit.eval.harness.label.{Rated, Verdicts}

/** Verdicts against live's draft and a question set's: each shadow's and reason's
  * [[Judgement.Of]], the shadows by name and the reasons in `Reason`'s order within one; and
  * `undecided`, the verdicts on a case either gate could not decide (a log has no answered row
  * of it, or a gate reads an answer its row lacks).
  */
final case class Judgement(reasons: Vector[Judgement.Of], undecided: Int)

object Judgement {

  /** `matched` of `of`. */
  final case class Matched(matched: Int, of: Int)

  /** The verdicts on the messages picked against `shadow` for `reason`, over the cases both
    * gates decided: `n` of them; how many live's draft (`live`) and the set's (`set`) each
    * decided as the verdict's [[Verdict.speaks]]; and of those whose `to` a side answered, how
    * many it read as the verdict's [[Verdict.toPerson]] (`liveTo`, `setTo`; `None` when that
    * side asks no `to`).
    */
  final case class Of(
      shadow: ShadowName,
      reason: Reason,
      n: Int,
      live: Int,
      set: Int,
      liveTo: Option[Matched],
      setTo: Option[Matched]
  )

  /** A side's `to` at least this reads as meant for a person. */
  val ToAt: Probability = Probability.clamped(0.5)

  /** `verdicts` against `live` and `set`, each case's first answered triage row of each: each
    * side's draft as its gate reads its answers ([[Drafts.of]]), and its `to` at [[ToAt]].
    * Counts only, by the reason each message was picked: nothing is weighted back by how often
    * a reason is picked.
    */
  def of(live: Drafting, set: Drafting, verdicts: Verdicts): Judgement = {
    def answered(d: Drafting): Map[CaseId, VectorMap[QuestionName, Answer]] =
      Drafts.firstAnswered(d.rows).map(x => x._1 -> x._3).toMap
    val (lives, sets) = (answered(live), answered(set))
    val decided = verdicts.cases.toVector.flatMap { (c, rated) =>
      for {
        theirs <- lives.get(c)
        answers <- sets.get(c)
        a <- live.gate.drafts(theirs)
        b <- set.gate.drafts(answers)
      } yield (
        rated,
        a,
        b,
        live.to.flatMap(yes(theirs, _)),
        set.to.flatMap(yes(answers, _))
      )
    }
    val groups = decided
      .groupBy((r, _, _, _, _) => (r.shadow, r.reason))
      .toVector
      .sortBy { case ((shadow, reason), _) => (ShadowName.value(shadow), reason.ordinal) }
    Judgement(
      groups.map { case ((shadow, reason), cases) =>
        def matched(f: (Verdict, Boolean, Boolean) => Boolean) =
          cases.count((r, a, b, _, _) => f(r.verdict, a, b))
        def to(
            side: Drafting,
            read: ((Rated, Boolean, Boolean, Option[Boolean], Option[Boolean])) => Option[Boolean]
        ) =
          side.to.map(_ =>
            Matched(
              cases.count(x => read(x).contains(x._1.verdict.toPerson)),
              cases.count(x => read(x).isDefined)
            )
          )
        Of(
          shadow,
          reason,
          cases.size,
          matched((v, a, _) => a == v.speaks),
          matched((v, _, b) => b == v.speaks),
          to(live, _._4),
          to(set, _._5)
        )
      },
      verdicts.cases.size - decided.size
    )
  }

  /** `q`'s yes/no in `answers` at [[ToAt]]; `None` when it is not a yes/no there. */
  private def yes(answers: VectorMap[QuestionName, Answer], q: QuestionName): Option[Boolean] =
    answers.get(q).collect { case Answer.YesNo(p) => Probability.clamped(p) >= ToAt }
}
