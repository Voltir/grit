package grit.core.period

import java.time.Instant

import grit.core.id.TurnSeq

/** What the classifier made of a period once it had been quiet for the settle window: asked
  * `at`, about the period as it stood with `last` its newest turn.
  */
final case class Verdict(at: Instant, last: TurnSeq, judgement: Judgement)

/** The classifier's answer to whether a quiet period is finished. */
enum Judgement {

  /** `model`'s probability of each option: finished, waiting on the person, waiting on
    * something else, unclear.
    */
  case Weighed(
      finished: Probability,
      onPerson: Probability,
      onOther: Probability,
      unclear: Probability,
      model: String
  )

  /** No answer, for `why`: the classifier was absent or failed, or its answer did not read. */
  case Unanswered(why: String)
}
