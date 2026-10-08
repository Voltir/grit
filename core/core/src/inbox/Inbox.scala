package grit.core.inbox

import java.time.Instant

import grit.core.approval.Approval
import grit.core.id.{CallSlot, EntryId, ScheduleId, SourceId, ToolCallId, TurnRef, WorkflowId}
import grit.core.identity.Account
import grit.core.job.Slot
import grit.core.message.Message
import grit.core.speech.Reach
import grit.core.spend.{DailyCap, Day, Spend}
import grit.core.store.{Origin, StoreError}

/** How an edge hands the engine work (ADR 0002). Every operation is idempotent, so an edge
  * that is unsure whether one happened repeats it.
  */
trait Inbox extends caps.SharedCapability {

  /** Records `message` from `origin`'s conversation, written through `by`, as the first entry of a
    * new turn, and returns that turn; an account not seen before is, unnamed, a new person's one
    * account. The conversation is created by `by` if it is new, at its room's label
    * ([[grit.core.store.Tx.roomLabel]]), a direct message's at its person's clearance then. A
    * message whose `source` id was already recorded for `origin` is not recorded again: its
    * existing turn is returned, with its first author, whatever was spent. A new message once the
    * day's recorded spend has reached the inbox's cap ([[grit.core.spend.Budget]]) is
    * [[InboxError.OverCap]]: nothing is recorded, its conversation not even created.
    * [[InboxError.Invalid]] for an [[grit.core.store.Origin.Direct]] whose account is not `by`: a
    * person writes only their own direct message. [[InboxError.Sealed]] for a new message in a
    * direct message's thread begun when its person was cleared for more than they are now: nothing
    * is recorded, and a message in a new thread begins one at their clearance now.
    */
  def ingest(
      origin: Origin,
      source: SourceId,
      message: Message.User,
      by: Account
  ): Either[InboxError, TurnRef]

  /** Records `text` from `origin`'s conversation, written through `by`, as heard: said where grit
    * listens, not to it ([[grit.core.store.Payload.Heard]]), as the first entry of a turn of
    * its own; an account not seen before is, unnamed, a new person's one account. The
    * conversation is created by `by` if it is new, at its room's label. It was
    * said `at`: its entry
    * is dated `at`, and a period it opens opens `at`, so a message heard late is as quiet as
    * it was. `reach` is where a reply to it could go and whom it names
    * ([[grit.core.speech.Reach]]); in a room quiet when it is recorded it keeps no address
    * ([[grit.core.speech.Reach.heardIn]]), so grit never replies there unasked. A message whose `source` was already recorded for
    * `origin`, heard or ingested, is not recorded again, and its first reach stands. Once
    * recorded, it is triaged, once ([[grit.core.triage.Tags]]); hearing it again triages it
    * if that was lost. Its turn runs only when grit drafts a reply to it
    * ([[grit.core.speech.Speech.decide]]). Never refused over the day's cap.
    * [[InboxError.Invalid]] for an [[grit.core.store.Origin.Direct]]: a direct message is
    * never heard.
    */
  def hear(
      origin: Origin,
      source: SourceId,
      text: String,
      by: Account,
      at: Instant,
      reach: Reach
  ): Either[InboxError, Unit]

  /** Records `text`, grit's post that `origin`'s thread begins with, as its conversation's
    * first entry ([[grit.core.store.Payload.Posted]]), dated `at`, its source `source`, made by
    * the hosted call at `request` ([[grit.core.store.ConversationStore.postedBy]]); the
    * conversation is created by `by`, at its room's label, an account not seen before kept as a
    * new person's one account. `true` when this call recorded it; `false`, recording
    * nothing, when anything is recorded for `origin` already, a repeat included. Never
    * triaged, never a turn that runs, never refused over the day's cap.
    * [[InboxError.Invalid]] for an [[grit.core.store.Origin.Direct]]: grit's posts begin no
    * direct message.
    */
  def posted(
      origin: Origin,
      source: SourceId,
      text: String,
      at: Instant,
      request: CallSlot,
      by: Account
  ): Either[InboxError, Boolean]

  /** Whether `origin`'s conversation exists: one is created with its first entry, so whether
    * anything is recorded for it.
    */
  def begun(origin: Origin): Either[InboxError, Boolean]

  /** The turn the message `source` from `origin` was recorded as by [[ingest]]; `None` when it
    * never was, or was heard ([[hear]]).
    */
  def ingested(origin: Origin, source: SourceId): Either[InboxError, Option[TurnRef]]

  /** Which of `sources` from `origin` are recorded, heard or ingested; none when its
    * conversation does not exist.
    */
  def recorded(origin: Origin, sources: Set[SourceId]): Either[InboxError, Set[SourceId]]

  /** Where `turn` has got to; [[InboxError.Unavailable]] when that cannot be read, which is
    * never read as the turn's end.
    */
  def progress(turn: TurnRef): Either[InboxError, Progress]

  /** Starts `turn` if it has not been started. A conversation runs one turn at a time,
    * oldest first; this returns once the turn is queued, not when it has run. A slot's run is
    * [[InboxError.SlotRun]], never started: only [[startSlot]] starts or restarts one.
    */
  def startTurn(turn: TurnRef): Either[InboxError, Unit]

  /** Starts what `schedule` has waiting at `now`, its job at `version` (`None`: the deployment
    * does not have its job), never refused over the day's cap (ADR 0029).
    *   - With no run in flight, its slot due ([[grit.core.job.Due]]) starts as a run at
    *     `version`: a turn of the slot's conversation ([[grit.core.job.Slot.origin]]), created
    *     by grit at its schedule's label, with [[grit.core.job.Slot.opening]], enqueued as its
    *     job's run. A once slot
    *     more than its grace past is missed, with or without its job.
    *   - With a run in flight, read by its workflow's status and its reply
    *     ([[grit.core.job.InFlight]], [[grit.core.job.Resume]]): one at another version, going
    *     or ended without a reply, is superseded by a run at `version`, of the same slot or,
    *     for a recurrence with a later slot due, of the latest; otherwise a run whose workflow
    *     has ended without a reply, however it ended, has `Failed`; one whose start was lost is
    *     enqueued again.
    * A slot already started at `version` is never started again ([[grit.core.job.Starting]]).
    */
  def startSlot(
      schedule: ScheduleId,
      version: Option[Int],
      now: Instant
  ): Either[InboxError, Slotted]

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

  /** The turn is `slot`'s run, which only [[Inbox.startSlot]] starts. */
  case SlotRun(slot: Slot)

  /** A new message not recorded: `spent` on `day` reached `cap`. Retrying does not help
    * before the next day. A person is told [[grit.core.spend.Budget.Refusal]], which names
    * neither.
    */
  case OverCap(spent: Spend, cap: DailyCap, day: Day)

  /** Refused as nothing an edge should ask, `why` saying what: retrying does not help. */
  case Invalid(why: String)

  /** `origin`, a direct message's thread, was begun when its person was cleared for more than
    * they are now: it takes no more messages ([[InboxError.SealedReply]] says so to them).
    */
  case Sealed(origin: Origin.Direct)
}

object InboxError {

  /** What an inbox reports for its store's failure `e`: [[Invalid]] for what the store holds
    * or is asked and cannot make good, which no retry changes; [[Unavailable]] for a database
    * that failed, or an entry another writer made meanwhile, which a retry finds.
    */
  def stored(e: StoreError): InboxError = e match {
    case StoreError.DatabaseError(cause) => Unavailable(cause)
    case StoreError.Invalid(why) => Invalid(why)
    case StoreError.DuplicateId(id) =>
      Unavailable(s"entry ${EntryId.value(id)} appeared mid-transaction")
  }

  /** What an edge tells a person, once, in a thread whose message was [[Sealed]]. */
  val SealedReply: String =
    "This thread began when you were cleared for more than you are now, so I can't continue " +
      "it here. Send me a new message, outside this thread."
}
