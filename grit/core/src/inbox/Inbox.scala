package grit.core.inbox

import grit.core.approval.Approval
import grit.core.id.{PrincipalId, SourceId, ToolCallId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.spend.{DailyCap, Day, Spend}
import grit.core.store.Origin

/** How an edge hands the engine work (ADR 0002). Every operation is idempotent, so an edge
  * that is unsure whether one happened repeats it.
  */
trait Inbox extends caps.SharedCapability {

  /** Records `message` from `origin`'s conversation, written by `by`, as the first entry of
    * a new turn, and returns that turn; the conversation is created by `by` if it is new. A
    * message whose `source` id was already recorded for `origin` is not recorded again: its
    * existing turn is returned, with its first author, whatever was spent. A new message once
    * the day's recorded spend has reached the inbox's cap ([[grit.core.spend.Budget]]) is
    * [[InboxError.OverCap]]: nothing is recorded, its conversation not even created.
    */
  def ingest(
      origin: Origin,
      source: SourceId,
      message: Message.User,
      by: PrincipalId
  ): Either[InboxError, TurnRef]

  /** Records `text` from `origin`'s conversation, written by `by`, as heard: said where grit
    * listens, not to it ([[grit.core.store.Payload.Heard]]). The edge never answers it, and
    * no turn runs for it. The conversation is created by `by` if it is new. A message whose
    * `source` was already recorded for `origin`, heard or ingested, is not recorded again.
    * Once recorded, it is triaged, once ([[grit.core.triage.Tags]]); hearing it again
    * triages it if that was lost. Never refused over the day's cap.
    */
  def hear(
      origin: Origin,
      source: SourceId,
      text: String,
      by: PrincipalId
  ): Either[InboxError, Unit]

  /** The turn the message `source` from `origin` was recorded as by [[ingest]]; `None` when it
    * never was, or was heard ([[hear]]).
    */
  def ingested(origin: Origin, source: SourceId): Either[InboxError, Option[TurnRef]]

  /** Where `turn` has got to; [[InboxError.Unavailable]] when that cannot be read, which is
    * never read as the turn's end.
    */
  def progress(turn: TurnRef): Either[InboxError, Progress]

  /** Starts `turn` if it has not been started. A conversation runs one turn at a time,
    * oldest first; this returns once the turn is queued, not when it has run.
    */
  def startTurn(turn: TurnRef): Either[InboxError, Unit]

  /** Answers the gated call `call` of the turn whose workflow is `workflow`, which asked
    * with a [[grit.core.store.Payload.Ask]] entry; the call runs only when `approval` is
    * [[Approval.Approved]]. Only the first answer to a call counts: any later one is
    * ignored, as is one sent after the turn stopped waiting. [[InboxError.NoSuchTurn]] when
    * no turn has that workflow.
    */
  def answer(workflow: WorkflowId, call: ToolCallId, approval: Approval): Either[InboxError, Unit]
}

/** A failure an edge is expected to handle. */
enum InboxError {

  /** The database rejected or could not complete the operation; retrying it is safe. */
  case Unavailable(cause: String)

  /** No turn has the workflow `workflow`. */
  case NoSuchTurn(workflow: WorkflowId)

  /** A new message not recorded: `spent` on `day` reached `cap`. Retrying does not help
    * before the next day. A person is told [[grit.core.spend.Budget.Refusal]], which names
    * neither.
    */
  case OverCap(spent: Spend, cap: DailyCap, day: Day)
}
