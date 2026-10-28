package grit.turn

import grit.act.phase.Faults
import grit.core.route
import grit.core.store.StoreError
import grit.core.visibility.Label

/** Why a turn ended without a reply, or without a summary. Recorded as a step's output and replayed, so each
  * case carries text rather than the error it came from.
  */
enum TurnFailure {

  /** No window: the store could not be read, or the window named entries it lacks. */
  case Assembly(reason: String)

  /** The provider produced no response, or none with the text it was asked for. */
  case Model(reason: String)

  /** The store could not be read or written. */
  case Store(reason: String)

  /** No pair the role may use is cleared for the turn's classification: its call is not made. */
  case Uncleared(classification: Label)

  /** A call binds otherwise than its round recorded: its tools changed between runs, as a
    * redeploy can change them. The call is not run.
    */
  case ToolsChanged(reason: String)
}

object TurnFailure {

  /** A phase's store failure, as a turn's steps record it: [[TurnFailure.Store]]. */
  given Faults[TurnFailure] = reason => TurnFailure.Store(reason)

  /** `error`, as a turn's steps record it: [[TurnFailure.Store]] saying
    * [[StoreError.described]].
    */
  private[turn] def ofStore(error: StoreError): TurnFailure = TurnFailure.Store(error.described)

  /** What `failure` says. */
  private[turn] def reason(failure: TurnFailure): String = failure match {
    case TurnFailure.Assembly(why) => why
    case TurnFailure.Model(why) => why
    case TurnFailure.Store(why) => why
    case TurnFailure.ToolsChanged(why) => why
    case TurnFailure.Uncleared(c) => route.Uncleared.reply(route.Uncleared(c))
  }
}
