package grit.core.inbox

import java.time.Instant

import grit.core.approval.Approval
import grit.core.id.{CallSlot, JobName, ScheduleId, SourceId, ToolCallId, TurnRef, WorkflowId}
import grit.core.identity.{Account, TestAccounts}
import grit.core.job.{InFlight, InMemorySchedules, LastRun, Slot, Starting}
import grit.core.message.Message
import grit.core.speech.{InMemorySpeechStore, Reach}
import grit.core.spend.Budget
import grit.core.store.{
  Conversation,
  Entry,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryPeriodStore,
  InMemoryPrincipals,
  InMemoryUsageLedger,
  Origin,
  Payload,
  StoreError,
  Tx
}
import grit.core.visibility.{Label, Visibility}
import grit.dbos.sql.TestTx

/** An in-memory [[Inbox]] for tests, keeping [[InboxContract]], over the in-memory stores it
  * is given: an ingested or heard message is an entry of its conversation, in its open period
  * ([[periods]], opened by the message when none is), its author told to `principals`, its
  * conversation created at its room's label as `visibility` gives it, and a new one is refused
  * once `ledger`'s spend today reaches `budget`'s cap, today being the day `ledger.now` falls
  * on. No turn runs: a test ends one with [[finish]].
  */
final class InMemoryInbox(
    val conversations: InMemoryConversationStore,
    val entries: InMemoryEntryStore,
    val principals: InMemoryPrincipals,
    val ledger: InMemoryUsageLedger,
    budget: Budget,
    visibility: Visibility
) extends Inbox {

  /** The conversations' periods, over [[entries]]. */
  val periods: InMemoryPeriodStore = new InMemoryPeriodStore(entries)

  /** The schedules whose slots [[startSlot]] starts. */
  val schedules: InMemorySchedules = new InMemorySchedules()

  /** Where each heard message could be answered, over [[entries]] and [[ledger]]. */
  val speech: InMemorySpeechStore = new InMemorySpeechStore(entries, ledger)

  /** The turns started, oldest first; a turn started twice is here once. */
  @caps.unsafe.untrackedCaptures
  var started = Vector.empty[TurnRef]

  /** The answers sent, in order. */
  @caps.unsafe.untrackedCaptures
  var answers = Vector.empty[(WorkflowId, ToolCallId, Approval)]

  /** When set, every call fails as the database would. */
  @caps.unsafe.untrackedCaptures
  var down = false

  @caps.unsafe.untrackedCaptures
  private var finished = Map.empty[TurnRef, Progress.Done]

  /** Ends `turn` with `reply` (written as its reply entry) and `outcome`. */
  def finish(turn: TurnRef, reply: Option[Message.Assistant], outcome: String): Unit = inTx {
    reply.foreach { r =>
      val next = entries.lockNext(turn.conversationId).fold(e => sys.error(e.toString), identity)
      val _ = entries.insert(
        Entry(
          turn.replyId,
          turn.conversationId,
          turn.turnSeq,
          None,
          next.seq,
          Payload.Message(r),
          java.time.Instant.EPOCH
        )
      )
    }
    finished = finished.updated(turn, Progress.Done(reply, outcome))
  }

  private def inTx[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)

  private def unavailable = Left(InboxError.Unavailable("the database is down"))

  /** `origin`'s conversation, as [[conversations]] reads it. */
  private def found(origin: Origin): Either[StoreError, Option[Conversation]] =
    inTx(conversations.find(origin))

  def ingest(
      origin: Origin,
      source: SourceId,
      message: Message.User,
      by: Account
  ): Either[InboxError, TurnRef] =
    origin match {
      case Origin.Direct(account, _) if (account: Account) != by =>
        Left(InboxError.Invalid("a direct message written through another's account"))
      case _ =>
        recorded(
          origin,
          source,
          Payload.Message(message),
          by,
          labelOf(origin),
          java.time.Instant.EPOCH,
          capped = true
        )
    }

  /** The label `origin`'s conversation is created at: a direct message's, its person's
    * clearance, its account's one-account person ([[TestAccounts.principal]]); any other's, its
    * room's.
    */
  private def labelOf(origin: Origin): Label = origin match {
    case Origin.Direct(account, _) => visibility.cleared(TestAccounts.principal(account))
    case Origin.Tui(_, _) | Origin.Slack(_, _, _) | Origin.Task(_, _) =>
      visibility.roomLabel(origin.room)
  }

  def hear(
      origin: Origin,
      source: SourceId,
      text: String,
      by: Account,
      at: java.time.Instant,
      reach: Reach
  ): Either[InboxError, Unit] =
    (origin match {
      case Origin.Direct(_, _) => Left(InboxError.Invalid("a direct message is never heard"))
      case _ =>
        recorded(
          origin,
          source,
          Payload.Heard(text),
          by,
          visibility.roomLabel(origin.room),
          at,
          capped = false
        )
    }).flatMap { turn =>
      inTx(speech.heard(turn, reach)) match {
        // A message recorded as a turn before is not heard, and keeps no reach.
        case Left(StoreError.Invalid(_)) | Right(()) => Right(())
        case Left(other) => Left(InboxError.stored(other))
      }
    }

  /** `payload` recorded as the first entry of a new turn of `origin`'s conversation, dated
    * `at` as the period it opens is, unless `source` was recorded before (its turn then) or,
    * when `capped`, the day's spend refuses it.
    */
  private def recorded(
      origin: Origin,
      source: SourceId,
      payload: Payload,
      by: Account,
      label: Label,
      at: java.time.Instant,
      capped: Boolean
  ): Either[InboxError, TurnRef] =
    if (down) unavailable
    else
      inTx {
        val result: Either[StoreError, Either[InboxError, TurnRef]] = for {
          known <- found(origin)
          before <- known.fold(Right(None): Either[StoreError, Option[Entry]])(c =>
            entries.get(InboundId.of(c.id, source))
          )
          refused <- before.fold(
            sealedOf(origin, known).fold(if (capped) overCap else Right(None))(w => Right(Some(w)))
          )(_ => Right(None))
          turn <- (before, refused) match {
            case (Some(e), _) => Right(Right(TurnRef(e.conversationId, e.turnSeq)))
            case (None, Some(why)) => Right(Left(why))
            case (None, None) =>
              for {
                conversation <- conversations.findOrCreate(origin, by, label)
                id = InboundId.of(conversation.id, source)
                next <- entries.lockNext(conversation.id)
                _ <- periods.openFor(conversation.id, next.turnSeq, at)
                _ <- entries.insert(
                  Entry(
                    id,
                    conversation.id,
                    next.turnSeq,
                    None,
                    next.seq,
                    payload,
                    at
                  )
                )
              } yield {
                principals.authored(id, by)
                Right(TurnRef(conversation.id, next.turnSeq))
              }
          }
        } yield turn
        result.left.map(InboxError.stored).flatMap(identity)
      }

  /** [[InboxError.Sealed]] for a direct message whose conversation, `known`, was created at a
    * label its person's clearance does not dominate; `None` otherwise.
    */
  private def sealedOf(
      origin: Origin,
      known: Option[Conversation]
  ): Option[InboxError] = origin match {
    case d @ Origin.Direct(_, _) =>
      known.filter(c => !labelOf(origin).dominates(c.label)).map(_ => InboxError.Sealed(d))
    case Origin.Tui(_, _) | Origin.Slack(_, _, _) | Origin.Task(_, _) => None
  }

  /** Why the spend on the day `ledger.now` falls on refuses a new message; `None` when it
    * does not.
    */
  private def overCap(using Tx^): Either[StoreError, Option[InboxError]] =
    budget.cap match {
      case None => Right(None)
      case Some(cap) =>
        val day = budget.today(ledger.now)
        ledger
          .on(day)
          .map(spent => Option.when(!budget.admits(spent))(InboxError.OverCap(spent, cap, day)))
    }

  def posted(
      origin: Origin,
      source: SourceId,
      text: String,
      at: Instant,
      request: CallSlot,
      by: Account
  ): Either[InboxError, Boolean] =
    if (down) unavailable
    else if (origin.place.direct) Left(InboxError.Invalid("grit's posts begin no direct message"))
    else
      inTx {
        val result: Either[StoreError, Boolean] = found(origin).flatMap {
          case Some(_) => Right(false)
          case None =>
            for {
              conversation <- conversations.findOrCreate(
                origin,
                by,
                visibility.roomLabel(origin.room)
              )
              next <- entries.lockNext(conversation.id)
              _ <- periods.openFor(conversation.id, next.turnSeq, at)
              _ <- entries.insert(
                Entry(
                  InboundId.of(conversation.id, source),
                  conversation.id,
                  next.turnSeq,
                  None,
                  next.seq,
                  Payload.Posted(text),
                  at
                )
              )
            } yield {
              conversations.posts = conversations.posts.updated(conversation.id, request)
              true
            }
        }
        result.left.map(InboxError.stored)
      }

  def begun(origin: Origin): Either[InboxError, Boolean] =
    if (down) unavailable else found(origin).map(_.nonEmpty).left.map(InboxError.stored)

  def ingested(origin: Origin, source: SourceId): Either[InboxError, Option[TurnRef]] =
    if (down) unavailable
    else
      inTx {
        found(origin)
          .flatMap {
            case None => Right(None)
            case Some(c) =>
              entries
                .get(InboundId.of(c.id, source))
                .map(_.collect { case e @ Entry(_, _, _, _, _, Payload.Message(_), _) =>
                  TurnRef(e.conversationId, e.turnSeq)
                })
          }
          .left
          .map(InboxError.stored)
      }

  def recorded(origin: Origin, sources: Set[SourceId]): Either[InboxError, Set[SourceId]] =
    if (down) unavailable
    else
      inTx {
        found(origin)
          .map {
            case None => Set.empty
            case Some(c) =>
              sources.filter(s => entries.get(InboundId.of(c.id, s)).exists(_.nonEmpty))
          }
          .left
          .map(InboxError.stored)
      }

  def progress(turn: TurnRef): Either[InboxError, Progress] =
    if (down) unavailable else Right(finished.getOrElse(turn, Progress.Open))

  /** As the SQL inbox decides, over [[schedules]]: a run is in flight while it is started and
    * not finished ([[finish]]), and replied when it finished with a reply.
    */
  def startSlot(
      schedule: ScheduleId,
      version: Option[Int],
      now: Instant
  ): Either[InboxError, Slotted] =
    if (down) unavailable
    else
      schedules.held(schedule) match {
        case None => Right(Slotted.Idle)
        case Some((job, rule, next, last, label)) =>
          val flight = last.flatMap((slot, running) => running.map(slot -> _)).map { (slot, v) =>
            val turn = runOf(Slot(schedule, slot), job, v)
            val state = turn match {
              case None => InFlight.Unknown
              case Some(t) =>
                finished.get(t) match {
                  case Some(done) => done.reply.fold(InFlight.Ended(v))(_ => InFlight.Replied)
                  case None if started.contains(t) => InFlight.Going(v)
                  case None => InFlight.Unknown
                }
            }
            (turn, state)
          }
          val kept = (e: Either[StoreError, Unit]) => e.left.map(InboxError.stored)
          Starting.of(
            rule,
            next,
            last.map((slot, _) => LastRun(slot, flight.map(_._2))),
            version,
            now
          ) match {
            case Starting.Idle => Right(Slotted.Idle)
            case Starting.Start(at, following, v, superseding) =>
              val slot = Slot(schedule, at)
              recorded(
                slot.origin(job),
                Slot.source(v),
                Payload.Message(slot.opening(job)),
                Account.Grit,
                label,
                now,
                capped = false
              )
                .map { turn =>
                  schedules.start(slot, v, following)
                  if (!started.contains(turn)) started = started :+ turn
                  if (superseding) Slotted.Superseding(turn, slot) else Slotted.Started(turn, slot)
                }
            case Starting.Restart =>
              flight.flatMap(_._1) match {
                case Some(turn) =>
                  if (!started.contains(turn)) started = started :+ turn
                  Right(Slotted.Restarted(turn))
                case None => Left(InboxError.Invalid("a run restarted, with none recorded"))
              }
            case Starting.Fail =>
              last match {
                case Some((slot, _)) =>
                  kept(schedules.failed(schedule, now))
                    .map(_ => Slotted.Failed(Slot(schedule, slot)))
                case None => Left(InboxError.Invalid("a run failed, with none started"))
              }
            case Starting.Miss(at) =>
              kept(schedules.missed(schedule, now)).map(_ => Slotted.Missed(Slot(schedule, at)))
            case Starting.Passed(following) =>
              last match {
                case Some((slot, _)) =>
                  kept(schedules.passed(schedule, following, now))
                    .map(_ => Slotted.Ran(Slot(schedule, slot)))
                case None => Left(InboxError.Invalid("a slot ran, with no run started"))
              }
          }
      }

  /** The turn `slot`'s run at `version`, `job`'s, was recorded as; `None` when it was not. */
  private def runOf(slot: Slot, job: JobName, version: Int): Option[TurnRef] =
    inTx {
      conversations.all.find(_.origin == slot.origin(job)).flatMap { c =>
        entries
          .get(InboundId.of(c.id, Slot.source(version)))
          .toOption
          .flatten
          .map(e => TurnRef(e.conversationId, e.turnSeq))
      }
    }

  def startTurn(turn: TurnRef): Either[InboxError, Unit] =
    if (down) unavailable
    else
      inTx(conversations.get(turn.conversationId)).left.map(InboxError.stored).flatMap { c =>
        c.flatMap(c => Slot.of(c.origin)) match {
          case Some(slot) => Left(InboxError.SlotRun(slot))
          case None =>
            if (!started.contains(turn)) started = started :+ turn
            Right(())
        }
      }

  def answer(workflow: WorkflowId, call: ToolCallId, approval: Approval): Either[InboxError, Unit] =
    if (down) unavailable
    else {
      answers = answers :+ ((workflow, call, approval))
      Right(())
    }
}

object InMemoryInbox {

  /** Empty in-memory stores, under `visibility`. */
  def fresh(
      budget: Budget = Budget(java.time.ZoneOffset.UTC, None),
      visibility: Visibility = Visibility.Shipped
  ): InMemoryInbox =
    new InMemoryInbox(
      new InMemoryConversationStore,
      new InMemoryEntryStore,
      new InMemoryPrincipals,
      new InMemoryUsageLedger,
      budget,
      visibility
    )
}
