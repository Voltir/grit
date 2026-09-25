package grit.core.classify

import grit.core.message.Usage

/** Capability to ask a classifier closed questions about a state: a conversation, a record,
  * a piece of text. Each call is one request with no memory of earlier ones. Shaped on Jev's
  * `POST /v1/systemone` (typesafe.ai): a JSON state, several named questions answered
  * together, a probability for every option.
  */
trait Classifier extends caps.SharedCapability {

  /** An answer for each of `questions` about `state`, of each question's kind, with what the
    * request consumed. Every id is answered, or the call fails. Ids are distinct.
    */
  def answer(
      state: ujson.Value,
      questions: Vector[(QuestionId, Question)]
  ): Either[ClassifierError, Answers]

  /** `questions` about `state`, answered together and read back into their typed form. */
  final def ask[T](state: ujson.Value, questions: Ask[T]): Either[ClassifierError, Answered[T]] = {
    val ids = questions.questions.map(_._1)
    val malformed = questions.questions.collectFirst {
      case (id, q) if Question.malformed(q) =>
        s"${QuestionId.value(id)}: a choice needs two or more distinct keys"
    }
    if (ids.distinct.size != ids.size)
      Left(ClassifierError.Invalid(s"a question id is used twice: ${ids.map(QuestionId.value)}"))
    else if (malformed.nonEmpty) Left(ClassifierError.Invalid(malformed.mkString))
    else
      answer(state, questions.questions).flatMap { a =>
        questions.read(a.answers).map(Answered(_, a.usage, a.model))
      }
  }
}

/** Every question's answer, what the request consumed, and the model that answered. */
final case class Answers(answers: Map[QuestionId, Answer], usage: Usage, model: String)

/** A typed answer, what the request consumed, and the model that answered. */
final case class Answered[T](value: T, usage: Usage, model: String)

/** A classifier call that produced no usable answer. */
enum ClassifierError {

  /** The classifier could not be reached or refused the request. */
  case Unavailable(cause: String)

  /** It replied, but not with an answer the question allows. */
  case Unreadable(why: String)

  /** The request was malformed before it was sent: a caller's mistake. */
  case Invalid(why: String)
}

object Classifier {

  /** A classifier that is not there: every call is `Unavailable` with `why`, at once. For
    * running without one, so code that asks need not branch on whether it can.
    */
  def none(why: String): Classifier =
    new Classifier {
      def answer(
          state: ujson.Value,
          questions: Vector[(QuestionId, Question)]
      ): Either[ClassifierError, Answers] = Left(ClassifierError.Unavailable(why))
    }
}
