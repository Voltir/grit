package grit.core.triage

import scala.collection.immutable.VectorMap

import grit.core.classify.Answer
import grit.core.id.QuestionName

/** What a shadow's answered call answered. */
enum ShadowAnswers {

  /** Triage's question in a wording, as a shadow kept it before that question was a set:
    * its answers in the order asked, the kind, waiting, durable and helps.
    */
  case Worded(answers: Vector[Answer])

  /** A question set's, each under its question's name, in the order asked. */
  case Named(answers: VectorMap[QuestionName, Answer])
}
