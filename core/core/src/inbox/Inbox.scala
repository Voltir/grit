package grit.core.inbox

import java.time.Instant

import grit.core.approval.Approval
import grit.core.id.{
  AgentType,
  CallSlot,
  EntryId,
  Href,
  PluginName,
  ScheduleId,
  SourceId,
  ToolCallId,
  TurnRef,
  TurnSeq,
  WorkflowId
}
import grit.core.identity.{Account, Principal}
import grit.core.job.Slot
import grit.core.message.{Cost, Message, SpendCap}
import grit.core.occasion.Occasion
import grit.core.speech.Reach
import grit.core.spend.{Budget, DailyCap, Day, Spend}
import grit.core.store.{Entry, Origin, Payload, StoreError, Tx, WentOn}
import grit.core.visibility.{Integrity, Label}

/** Where a plugin's occasion is recorded as a turn of its own: the one inbox write a keeping
  * job's occasion move makes ([[grit.core.act.Keeping.occasion]]).
  */
trait OccasionInbox extends caps.SharedCapability {

  /** Records `occasion`, `plugin`'s, as the first entry of a new turn of `origin`'s conversation
    * ([[grit.core.store.Payload.Occasion]]), grit's own, dated `at`, believed at `believed`, its
    * source [[grit.core.occasion.OccasionKey.source]], and returns that turn; the turn recorded
    * before when that source was in that conversation, whatever this occasion holds, so a step that re-runs gets it
    * back. Never refused over the day's cap. [[InboxError.Invalid]] for an
    * [[grit.core.store.Origin.Direct]], a spawned child's ([[InboxError.unspawned]]), and when
    * the store holds what it cannot read.
    */
  def occasion(
      origin: Origin,
      plugin: PluginName,
      occasion: Occasion,
      at: Instant,
      believed: Integrity
  ): Either[InboxError, TurnRef]
}

/** Where a spawned child is recorded and started, its end told, and its reply read. */
trait SpawnInbox extends caps.SharedCapability {

  /** Records `brief` as grit's note beginning the first turn of `origin`'s new conversation
    * ([[grit.core.store.Payload.Spawn]]) and starts that turn, as [[Inbox.startTurn]] does,
    * through the account that wrote `origin`'s parent's first entry, so the child answers its
    * parent's asker; the conversation made at its room's label, the brief believed at
    * `believed`. A rerun returns the same turn, recorded once, and starts it, so a rerun after a
    * failed start starts it: `origin`'s place is the key. [[InboxError.Above]] when the room's
    * label does not dominate `classification`, the parent's; [[InboxError.OverCap]] as
    * [[Inbox.ingest]] refuses a new message; [[InboxError.Invalid]] when no account wrote the
    * parent's first entry (it has none, or grit wrote it), or `origin` is within a direct
    * message's room. A refusal records nothing.
    */
  def spawned(
      origin: Origin.Spawned,
      agent: AgentType,
      brief: String,
      spendCap: SpendCap,
      believed: Integrity,
      classification: Label
  ): Either[InboxError, TurnRef]

  /** How `child` ended, from its workflow alone; `None` until it has. Its reply is not read. */
  def ended(child: TurnRef): Either[InboxError, Option[TurnEnding]]

  /** `child`'s reply, as the transaction's clearance may read it: [[SpawnReply.Silent]] too for
    * a child still running, so ask [[ended]] first.
    */
  def reply(child: TurnRef)(using Tx^): Either[StoreError, SpawnReply]

  /** What `turn`'s conversation's recorded calls cost: a child's, or a parent's. */
  def spent(turn: TurnRef)(using Tx^): Either[StoreError, Cost]

  /** The spend caps `parent`'s children were spawned with, in the order they were, whatever the
    * transaction's clearance: a cap is its type's, never its child's words, and a child kept
    * above the parent's clearance still counts against what the parent may grant.
    */
  def granted(parent: TurnRef)(using Tx^): Either[StoreError, Vector[SpendCap]]

  /** Rings `child`'s parent, named by its origin, on [[SpawnInbox.topic]], that it ended as
    * `ending`, the ring's message its [[TurnEnding.word]]; once per child, however often called,
    * the first ring's ending kept. [[InboxError.Invalid]] when `child`'s conversation is not a
    * spawned child's; [[InboxError.NoSuchTurn]] when its parent never ran.
    */
  def returned(child: TurnRef, ending: TurnEnding): Either[InboxError, Unit]
}

object SpawnInbox {

  /** The topic a child's ring reaches its parent's workflow on: one per child. */
  def topic(child: TurnRef): String = s"returned:${WorkflowId.value(child.workflowId)}"
}

/** A child's reply as one transaction may read it. */
enum SpawnReply extends caps.Pure {

  /** Its reply's text, believed as its entry was written. */
  case Said(text: String, believed: Integrity)

  /** No reply is kept. */
  case Silent

  /** Its reply is above the transaction's clearance. */
  case Unread
}

/** How an edge hands the engine work (ADR 0002). Every operation is idempotent, so an edge
  * that is unsure whether one happened repeats it.
  */
trait Inbox extends OccasionInbox {

  /** Records `message` from `origin`'s conversation, written through `by`, as the first entry of a
    * new turn, and returns that turn; an account not seen before is, unnamed, a new person's one
    * account. The conversation is created by `by` if it is new, at its room's label
    * ([[grit.core.store.Tx.roomLabel]]), a direct message's at its person's clearance then. A
    * message whose `source` id was already recorded for `origin` is not recorded again: its
    * existing turn is returned, with its first author, whatever was spent. A new message once the
    * day's recorded spend has reached the inbox's cap ([[grit.core.spend.Budget]]) is
    * [[InboxError.OverCap]]: nothing is recorded, its conversation not even created.
    * [[InboxError.Invalid]] for an [[grit.core.store.Origin.Direct]] whose account is not `by`: a
    * person writes only their own direct message; and for a spawned child's, whatever `source`
    * is ([[InboxError.unspawned]]). [[InboxError.Sealed]] for a new message in a
    * direct message's thread begun when its person was cleared for more than they are now: nothing
    * is recorded, and a message in a new thread begins one at their clearance now. `link` opens
    * the message where it was said, as its edge links it (`None` from an edge that links none);
    * the first recording's stands, read back with its author ([[grit.core.store.Principals]]),
    * and one over [[grit.core.id.Href.Chars]] characters is not kept.
    */
  def ingest(
      origin: Origin,
      source: SourceId,
      message: Message.User,
      by: Account,
      link: Option[Href]
  ): Either[InboxError, TurnRef]

  /** Records grit's note beginning turn `turn` of the task turn `task` of `origin`'s
    * conversation began ([[grit.core.store.Payload.Continuation]]), after the turn before it
    * ended as `reason` says, as the first entry of a new turn written through `by`, and returns
    * that turn. `by` must have written the task's first message, so the turn answers the task's
    * asker at the task's clearance (ADR 0058); it is believed no higher than that message, and
    * no higher than [[grit.core.store.WentOn.believed]] allows `reason`. A
    * `source` already recorded for `origin` returns its turn. [[InboxError.OverCap]] as
    * [[ingest]] refuses a new message; [[InboxError.Invalid]] when `origin` is a spawned
    * child's ([[InboxError.unspawned]]) or has no conversation, `task` has no first entry in it,
    * or `by` did not write that entry. A
    * conversation whose period closed since the task still takes it.
    */
  def continued(
      origin: Origin,
      source: SourceId,
      task: TurnSeq,
      turn: Int,
      reason: WentOn,
      by: Account
  ): Either[InboxError, TurnRef]

  /** The refusal a new message would get now over the day's cap ([[InboxError.OverCap]]), by
    * the rule [[ingest]] applies; `None` under it, or with no cap.
    */
  def overCap(): Either[InboxError, Option[InboxError.OverCap]]

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
    * never heard; and for a spawned child's ([[InboxError.unspawned]]). `link` is kept as
    * [[ingest]] keeps it.
    */
  def hear(
      origin: Origin,
      source: SourceId,
      text: String,
      by: Account,
      at: Instant,
      reach: Reach,
      link: Option[Href]
  ): Either[InboxError, Unit]

  /** Records `text`, grit's post that `origin`'s thread begins with, as its conversation's
    * first entry ([[grit.core.store.Payload.Posted]]), dated `at`, its source `source`, made by
    * the hosted call at `request` ([[grit.core.store.ConversationStore.postedBy]]); or, in a
    * channel's top level ([[grit.core.store.Origin.Room]]), as the first entry of a turn of its
    * own after what it holds, its call not kept. The conversation is created by `by`, at its
    * room's label, an account not seen before kept as a new person's one account. `true` when
    * this call recorded it; `false`, recording nothing, when `source` is recorded for `origin`
    * already (a repeat of this call included), or, outside a channel's top level, when
    * anything is recorded for `origin`. Never
    * triaged, never a turn that runs, never refused over the day's cap.
    * [[InboxError.Invalid]] for an [[grit.core.store.Origin.Direct]]: grit's posts begin no
    * direct message; and for a spawned child's ([[InboxError.unspawned]]).
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

  /** Answers the gated call `call` of the turn whose workflow is `workflow`, which asked with a
    * [[grit.core.store.Payload.Ask]] entry, as `by`; the call runs only when `approval` is
    * [[Approval.Approved]]. Refused [[InboxError.NotAsker]] unless `by` is an account of the
    * turn's asker ([[grit.core.store.Askers.of]]); [[InboxError.Closed]] when the call has no
    * open ask: it was settled, or its ask's `until` has passed by the database's clock;
    * [[InboxError.NoSuchTurn]] when no turn has that workflow. Of two answers sent before the
    * turn reads either, only the first counts, and the second is still `Right`, so an edge that
    * reports "confirmed" on `Right` says so to a second click whatever the first was. Its reads
    * (the ask entry, the asker) are opened for `Subject.Turn` of the workflow's turn.
    */
  def answer(
      workflow: WorkflowId,
      call: ToolCallId,
      approval: Approval,
      by: Account
  ): Either[InboxError, Unit]
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

  /** A child of a turn classified `classification` would be made in a room labelled `room`,
    * which does not dominate it, and would write what it was told down to it. Retrying does not
    * help.
    */
  case Above(classification: Label, room: Label)

  /** `by` is no account of the turn's asker: only the person a turn answers may answer its asks. */
  case NotAsker(by: Account)

  /** The call `call` of `workflow` has no open ask: it was answered and settled, or its wait ran out. */
  case Closed(workflow: WorkflowId, call: ToolCallId)
}

object InboxError {

  /** Why [[SpawnInbox.spawned]] refuses a child whose parent's first entry no account wrote. */
  val Unwritten: String = "a child is spawned only from a turn a person's account began"

  /** Why `by` may not answer the gated call `call` of `workflow`'s turn, whose entries are
    * `entries`, as its transaction reads them, and whose asker is `asker`, at `now`, as
    * [[Inbox.answer]] refuses it: [[NoSuchTurn]] for a turn with no entries, [[NotAsker]] for
    * an account not among the asker's, [[Closed]] for a call with no ask waiting at `now`
    * ([[grit.core.store.Payload.waiting]]); `None` when they may.
    */
  def answering(
      workflow: WorkflowId,
      call: ToolCallId,
      by: Account,
      entries: Vector[Entry],
      asker: Option[Principal],
      now: Instant
  ): Option[InboxError] = {
    val theirs = asker match {
      case Some(Principal.Person(_, accounts)) => accounts.exists(_.account == by)
      case Some(Principal.Grit) | None => false
    }
    if (entries.isEmpty) Some(NoSuchTurn(workflow))
    else if (!theirs) Some(NotAsker(by))
    else if (!Payload.waiting(entries.map(_.payload), now).exists(_.call == call))
      Some(Closed(workflow, call))
    else None
  }

  /** What an inbox reports for its store's failure `e`: [[Invalid]] for what the store holds
    * or is asked and cannot make good, which no retry changes; [[Unavailable]] for a database
    * that failed, or an entry another writer made meanwhile, which a retry finds.
    */
  def stored(e: StoreError): InboxError = e match {
    case StoreError.DatabaseError(cause) => Unavailable(cause)
    case StoreError.Invalid(why) => Invalid(why)
    case StoreError.Full(bound) => Invalid(s"a plugin's $bound documents are all it may hold")
    case w: StoreError.Worked => Invalid(w.described)
    case StoreError.DuplicateId(id) =>
      Unavailable(s"entry ${EntryId.value(id)} appeared mid-transaction")
  }

  /** [[Invalid]] for a spawned child's conversation ([[Origin.Spawned]]), whose one turn its
    * spawn records ([[SpawnInbox.spawned]]): what every door that begins a turn answers it, before
    * it reads anything.
    */
  def unspawned(origin: Origin): Either[InboxError, Unit] = origin match {
    case Origin.Spawned(_, _, _) => Left(Invalid("a spawned child's conversation is its one turn"))
    case Origin.Tui(_, _) | Origin.Slack(_, _, _) | Origin.Room(_, _) | Origin.Thread(_, _, _) |
        Origin.Task(_, _) | Origin.Work(_, _) | Origin.Direct(_, _) =>
      Right(())
  }

  /** What an edge tells a person, once, in a thread whose message was [[Sealed]]. */
  val SealedReply: String =
    "This thread began when you were cleared for more than you are now, so I can't continue " +
      "it here. Send me a new message, outside this thread."

  /** What a person is told of a message `error` kept from being taken: for one over the
    * day's cap, the one refusal every edge gives ([[grit.core.spend.Budget.Refusal]]), which
    * names no cost; otherwise that it was not sent, and why.
    */
  def told(error: InboxError): String = error match {
    case OverCap(_, _, _) => Budget.Refusal
    case other => s"not sent: $other"
  }
}
