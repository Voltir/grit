package grit.core

/** How an edge hands the engine work (ADR 0002). Both operations are idempotent, so an
  * edge that is unsure whether one happened repeats it.
  */
trait Inbox extends caps.SharedCapability {

  /** Records `message` from `origin`'s conversation as the first entry of a new turn, and
    * returns that turn. A message whose `source` id was already recorded for `origin` is
    * not recorded again: its existing turn is returned.
    */
  def ingest(origin: Origin, source: SourceId, message: Message.User): Either[InboxError, TurnRef]

  /** Starts `turn` if it has not been started. A conversation runs one turn at a time,
    * oldest first; this returns once the turn is queued, not when it has run.
    */
  def startTurn(turn: TurnRef): Either[InboxError, Unit]
}

/** A failure an edge is expected to handle; retrying the same call is always safe. */
enum InboxError {

  /** The database rejected or could not complete the operation. */
  case Unavailable(cause: String)
}
