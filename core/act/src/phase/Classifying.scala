package grit.act.phase

import scala.concurrent.duration.FiniteDuration

import grit.core.classify.{Classifier, ClassifierError, Reply, Request}
import grit.core.clock.Clock

/** A judgment's phase: a classifier's answers, retried while it is unavailable. */
object Classifying {

  /** `request`'s reply from `classifier`, asked again after each of [[Asking.Retries]] while
    * it is `Unavailable`, waiting on `clock`. The last failure, or an `Unreadable` one, the
    * reply's answers' included, has its cause followed by " (after n tries)" when there was
    * more than one.
    */
  def replied(
      classifier: Classifier^,
      request: Request,
      clock: Clock^
  ): Either[ClassifierError, Reply] = {
    def attempt(waits: List[FiniteDuration], tries: Int): Either[ClassifierError, Reply] =
      (classifier.replied(request), waits) match {
        case (Left(ClassifierError.Unavailable(_)), wait :: rest) =>
          clock.sleep(wait)
          attempt(rest, tries + 1)
        case (Left(error), _) => Left(after(error, tries))
        case (Right(reply), _) => Right(reply.failing(after(_, tries)))
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
