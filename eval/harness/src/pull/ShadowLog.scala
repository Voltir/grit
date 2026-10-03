package grit.eval.harness.pull

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName
import grit.eval.harness.log.{Log, Weights}

/** A shadow's pulled answers, by the form its answered rows were kept in
  * ([[grit.core.triage.ShadowAnswers]]); a shadow none of whose rows answered is `Worded`.
  */
enum ShadowLog {

  /** Triage's question in a wording: a log the scorers read beside live triage's. */
  case Worded(log: Log[Vector[Weights]])

  /** A question set's: each row's answers under their names, its header's `questions` the
    * names its first answered row answered.
    */
  case Named(log: Log[VectorMap[QuestionName, Answer]])

  /** Refused, no log: `worded` of its answered rows were a wording's and `named` a question
    * set's. A shadow's name asks one question for its life, so a name declared first for one
    * and then the other is an error to fix in the deployment, not a log to read.
    */
  case Mixed(worded: Int, named: Int)
}
