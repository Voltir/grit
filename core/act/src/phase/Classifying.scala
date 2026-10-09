package grit.act.phase

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.{Answers, Classifier, ClassifierError, Request}
import grit.core.clock.Clock

/** A judgment's phase: a classifier's answers, retried while it is unavailable. */
object Classifying {

  /** `request` asked of `classifier`, again after each of [[Asking.Retries]] while it is
    * `Unavailable`, waiting on `clock`. The last failure, or an `Unreadable` one, is `Left`
    * with its cause followed by " (after n tries)" when there was more than one.
    */
  def answers(
      classifier: Classifier^,
      request: Request,
      clock: Clock^
  ): Either[ClassifierError, Answers] = {
    def attempt(waits: List[FiniteDuration], tries: Int): Either[ClassifierError, Answers] =
      (classifier.answers(request), waits) match {
        case (Left(ClassifierError.Unavailable(_)), wait :: rest) =>
          clock.sleep(wait)
          attempt(rest, tries + 1)
        case (Left(error), _) => Left(after(error, tries))
        case (Right(answers), _) => Right(answers)
      }
    attempt(Asking.Retries, 1)
  }

  private def after(error: ClassifierError, tries: Int): ClassifierError = {
    val told = if (tries == 1) "" else s" (after $tries tries)"
    error match {
      case ClassifierError.Unavailable(cause) => ClassifierError.Unavailable(cause + told)
      case ClassifierError.Unreadable(why) => ClassifierError.Unreadable(why + told)
    }
  }
}
