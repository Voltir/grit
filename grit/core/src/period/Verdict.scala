package grit.core.period

import java.time.Instant

import grit.core.id.TurnSeq

/** What the classifier made of a period once it had been quiet for the settle window: asked
  * `at`, about the period as it stood with `last` its newest turn.
  */
final case class Verdict(at: Instant, last: TurnSeq, judgement: Judgement)

/** The classifier's answer to whether anyone is waiting on anything in a quiet period. */
enum Judgement {

  /** `model`'s probability that nobody is waiting on anything (`nobody`), that the person
    * is (`onPerson`), and that something else is (`onOther`).
    */
  case Weighed(nobody: Probability, onPerson: Probability, onOther: Probability, model: String)

  /** No answer, for `why`: the classifier was absent or failed, or its answer did not read. */
  case Unanswered(why: String)
}
