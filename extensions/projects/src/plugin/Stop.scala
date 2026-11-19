package grit.projects.plugin

import grit.core.plugin.CommandOutcome
import grit.core.store.StoreError

/** Why a command stopped: its store failed, or it refused, in words its asker reads. */
private[plugin] enum Stop extends caps.Pure {
  case Store(error: StoreError)
  case Refused(why: String)
}

private[plugin] object Stop {

  /** `step`'s answer as a command's outcome: its text done, its refusal refused, its store's
    * failure failed.
    */
  def answered(step: Either[Stop, String]): Either[StoreError, CommandOutcome] =
    step match {
      case Right(text) => Right(CommandOutcome.Done(text))
      case Left(Stop.Refused(why)) => Right(CommandOutcome.Refused(why))
      case Left(Stop.Store(e)) => Left(e)
    }

  /** `e`'s reason as a refusal. */
  def refuse[A](e: Either[String, A]): Either[Stop, A] = e.left.map(Stop.Refused(_))

  /** `e`'s failure as the store's. */
  def stored[A](e: Either[StoreError, A]): Either[Stop, A] = e.left.map(Stop.Store(_))
}
