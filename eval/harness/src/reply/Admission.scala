package grit.eval.harness.reply

import grit.core.context.Results
import grit.core.period.Probability
import grit.turn.{TurnRecord, TurnRelevance}

import TurnRelevance.Verdict

/** Which of a recorded search's results a rebuild packs, tried over the verdicts a turn
  * recorded with no classifier asked: of its first `judged` results, each whose recorded
  * probability is `pass` or more, and each left unjudged.
  */
final case class Admission private (judged: Int, pass: Probability) {

  /** What this admits of `retrieval`, in its results' order. */
  def admitted(retrieval: TurnRecord.Retrieval): Results =
    TurnRelevance.admitted(
      retrieval.results.copy(ranked = retrieval.results.ranked.take(judged)),
      retrieval.verdicts.map {
        case (r, Verdict.Passed(p)) => r -> again(p)
        case (r, Verdict.Rejected(p)) => r -> again(p)
        case unjudged => unjudged
      }
    )

  private def again(p: Probability): Verdict =
    if (p >= pass) Verdict.Passed(p) else Verdict.Rejected(p)
}

object Admission {

  /** The turn's own: its first [[TurnRelevance.Judged]], passing at [[TurnRelevance.Pass]]. */
  val Shipped: Admission = Admission(TurnRelevance.Judged, TurnRelevance.Pass)

  /** `Left` when `judged` is outside 0 to [[TurnRelevance.Judged]]: past the turn's own, no
    * verdict was recorded, so judging more needs a classifier asked again.
    */
  def of(judged: Int, pass: Probability): Either[String, Admission] =
    Either.cond(
      judged >= 0 && judged <= TurnRelevance.Judged,
      Admission(judged, pass),
      s"judged must be 0 to ${TurnRelevance.Judged}, the most a turn judged; was $judged"
    )
}
