package grit.core.classify

import grit.core.message.Usage

/** Capability to ask a classifier closed questions about a state: a conversation, a record,
  * a piece of text. Each call is one request with no memory of earlier ones. Shaped on Jev's
  * `POST /v1/systemone` (typesafe.ai): a JSON state, several questions answered together, a
  * probability for every option.
  */
trait Classifier extends caps.SharedCapability {

  /** `questions` about `state`, sent as one request; `Unreadable` when the reply holds other
    * than one answer per question, or an answer does not read: [[answers]], then [[Ask.answered]].
    */
  final def ask[S: StateJson, T](
      state: S,
      questions: Ask[S, T]
  ): Either[ClassifierError, Answered[T]] =
    answers(Request.of(state, questions)).flatMap(questions.answered)

  /** The classifier's answers to `request`, unread into any type, with what the request
    * consumed and the model that answered. `Unreadable` only when there is not one answer per
    * question, in the questions' order and of each one's kind; whether a choice names a key,
    * or a score weighs each level, is [[Ask.read]]'s to say. [[ask]] is the two together.
    */
  final def answers(request: Request): Either[ClassifierError, Answers] =
    replied(request).flatMap(_.answers)

  /** The classifier's reply to `request`, what it consumed and the model that answered kept
    * however its answers read: `Left` only when there was no reply. What a call's cost is
    * read from; [[answers]] is its answers.
    */
  final def replied(request: Request): Either[ClassifierError, Reply] =
    answer(request.state, request.questions).map { a =>
      Reply(
        a.usage,
        a.model,
        Ask
          .counted(request.questions, a.answers)
          .orElse(Ask.kinds(request.questions, a.answers))
          .toLeft(a)
      )
    }

  /** One answer per question, in the questions' order and of each one's kind. Called only by
    * [[replied]], with a request's questions, never empty.
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

/** A classifier's reply: what its request consumed, the model that answered, and its
  * `answers`, `Unreadable` when they are not one per question, in the questions' order and of
  * each one's kind ([[Classifier.answers]]).
  */
final case class Reply private[classify] (
    usage: Usage,
    model: String,
    answers: Either[ClassifierError, Answers]
) {

  /** This reply, its answers' failure, when they do not read, in `f`'s words. */
  def failing(f: ClassifierError -> ClassifierError): Reply = copy(answers = answers.left.map(f))
}

/** A typed answer, what the request consumed, and the model that answered. */
final case class Answered[T](value: T, usage: Usage, model: String)

/** A classifier call that produced no usable answer. */
enum ClassifierError {

  /** The classifier could not be reached, or was too busy to answer: asking again later may
    * succeed.
    */
  case Unavailable(cause: String)

  /** The classifier was reached and turned the request down: asking it again would not help. */
  case Refused(cause: String)

  /** It replied, but not with an answer the questions allow. */
  case Unreadable(why: String)

  /** Which of these it is, without its words. */
  def kind: ClassifierError.Kind = this match {
    case Unavailable(_) => ClassifierError.Kind.Unavailable
    case Refused(_) => ClassifierError.Kind.Refused
    case Unreadable(_) => ClassifierError.Kind.Unreadable
  }

  /** Its kind and its words, as a record of an unanswered question keeps them:
    * `unavailable: <cause>`, `refused: <cause>` or `unreadable: <why>`.
    */
  def said: String = this match {
    case Unavailable(cause) => s"unavailable: $cause"
    case Refused(cause) => s"refused: $cause"
    case Unreadable(why) => s"unreadable: $why"
  }
}

object ClassifierError {

  /** What kind of failure a [[ClassifierError]] is, kept where its words may not be. */
  enum Kind {
    case Unavailable, Refused, Unreadable
  }
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
