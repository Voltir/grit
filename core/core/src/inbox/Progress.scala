package grit.core.inbox

import grit.core.message.Message

/** Where a turn has got to, as an edge reads it through the [[Inbox]]. */
enum Progress {

  /** Not yet run to its end: never started, queued, or running. */
  case Open

  /** Run to its end: its reply (the entry [[grit.core.id.TurnRef.replyId]] names), or `None`
    * when it ended with none, and the turn's outcome as its workflow returned it, or why it
    * returned none.
    */
  case Done(reply: Option[Message.Assistant], outcome: String)

  /** How the turn ended, as a `Done`'s outcome says; [[TurnEnding.Otherwise]] while `Open`. */
  def ending: TurnEnding = this match {
    case Done(_, outcome) => TurnEnding.of(outcome)
    case Open => TurnEnding.Otherwise
  }
}

/** How a turn ended, as its workflow's outcome says it ([[Progress.Done.ending]]). */
enum TurnEnding {

  /** Its requests reached their limit, so its last calls were cut short (ADR 0052), and its
    * model did not declare its task done (ADR 0058): its task goes on in a new turn only when
    * its driver continues it ([[Inbox.continued]]).
    */
  case AtLimit

  /** Any other way: it answered within its limit, declared its task done at it, ran out of
    * calls, failed or drafted.
    */
  case Otherwise
}

object TurnEnding {

  /** What the outcome of a turn that ended at its limit ends with. */
  val AtLimitSaid: String = "; ended at its limit"

  /** How the outcome `outcome` says its turn ended: [[AtLimit]] when it ends with
    * [[AtLimitSaid]], else [[Otherwise]].
    */
  def of(outcome: String): TurnEnding =
    if (outcome.endsWith(AtLimitSaid)) AtLimit else Otherwise
}
