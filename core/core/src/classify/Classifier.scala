package grit.core.classify

import grit.core.message.Usage

/** Capability to ask a classifier closed questions about a state: a conversation, a record,
  * a piece of text. Each call is one request with no memory of earlier ones. Shaped on Jev's
  * `POST /v1/systemone` (typesafe.ai): a JSON state, several questions answered together, a
  * probability for every option.
  */
trait Classifier extends caps.SharedCapability {

  /** `questions` about `state`, sent as one request; `Unreadable` when the reply holds other
    * than one answer per question, or an answer does not read (see [[Ask.choice]]).
    */
  final def ask[S: StateJson, T](
      state: S,
      questions: Ask[S, T]
  ): Either[ClassifierError, Answered[T]] = {
    val request = Request.of(state, questions)
    answer(request.state, request.questions).flatMap { a =>
      if (a.answers.size != questions.questions.size)
        Left(
          ClassifierError.Unreadable(
            s"${a.answers.size} answers to ${questions.questions.size} questions"
          )
        )
      else questions.read(a.answers).map(Answered(_, a.usage, a.model))
    }
  }

  /** One answer per question, in the questions' order and of each one's kind. Called only by
    * [[ask]], which never sends an empty `questions`.
    */
  protected def answer(
      state: ujson.Value,
      questions: Vector[Question]
  ): Either[ClassifierError, Answers]
}

/** Every question's answer in the questions' order, what the request consumed, and the model
  * that answered.
  */
final case class Answers(answers: Vector[Answer], usage: Usage, model: String)

/** A typed answer, what the request consumed, and the model that answered. */
final case class Answered[T](value: T, usage: Usage, model: String)

/** A classifier call that produced no usable answer. */
enum ClassifierError {

  /** The classifier could not be reached or refused the request. */
  case Unavailable(cause: String)

  /** It replied, but not with an answer the questions allow. */
  case Unreadable(why: String)
}

object Classifier {

  /** `inner`, each request first handed to `f` with `ask`, which asks `inner` that request
    * once each time it is called: `f`'s result is the answer. For caching, timing or
    * recording what a classifier is asked; `f` that never calls `ask` answers alone.
    */
  def around(inner: Classifier^)(f: Around^): Classifier^ =
    new Classifier {
      protected def answer(
          state: ujson.Value,
          questions: Vector[Question]
      ): Either[ClassifierError, Answers] =
        f(Request(state, questions), () => inner.answer(state, questions))
    }

  /** What [[around]] hands each request to: the request, and `ask`, which asks the classifier
    * it wraps. A capability shared like the classifier it becomes part of, so a lambda that
    * captures only shared capabilities is one.
    */
  trait Around extends caps.SharedCapability {
    def apply(
        request: Request,
        ask: () => Either[ClassifierError, Answers]
    ): Either[ClassifierError, Answers]
  }

  /** A classifier that is not there: every call is `Unavailable` with `why`, at once. For
    * running without one, so code that asks need not branch on whether it can.
    */
  def none(why: String): Classifier =
    new Classifier {
      protected def answer(
          state: ujson.Value,
          questions: Vector[Question]
      ): Either[ClassifierError, Answers] = Left(ClassifierError.Unavailable(why))
    }
}
