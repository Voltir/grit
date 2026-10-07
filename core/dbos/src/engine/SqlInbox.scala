package grit.dbos.engine

import java.time.Instant
import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.approval.Approval
import grit.core.id.{
  CallSlot,
  ConversationId,
  EntryId,
  JobName,
  PrincipalId,
  ScheduleId,
  SourceId,
  ToolCallId,
  TriageRef,
  TurnRef,
  WorkflowId
}
import grit.core.inbox.{InboundId, Inbox, InboxError, Progress, Slotted}
import grit.core.job.{InFlight, LastRun, Slot, Starting}
import grit.core.message.Message
import grit.core.speech.{Reach, SpeechStore}
import grit.core.spend.{Budget, Spending}
import grit.core.stitch.Opening
import grit.core.store.{
  ConversationStore,
  Entry,
  EntryStore,
  Origin,
  Payload,
  PeriodStore,
  StoreError,
  Tx
}
import grit.core.visibility.{Label, Visibility}
import grit.dbos.sql.{SqlEntryStore, SqlSchedules}
import grit.dbos.workflow.{Runs, Stitches, Triages, Turns}

import dev.dbos.transact.exceptions.DBOSNonExistentWorkflowException
import dev.dbos.transact.workflow.WorkflowState
import dev.dbos.transact.{DBOSClient, EnqueueOptions}

/** [[Inbox]] over Postgres alone, so an edge in another process can use it: ingest is one
  * short transaction, which refuses a new message once `spending` says today's spend has
  * reached `budget`'s cap (today by this machine's clock, in `budget`'s zone), and a turn is
  * started by enqueueing it through `client`. A conversation it creates for an edge's origin is
  * labelled as `visibility` labels its room.
  */
final class SqlInbox(
    dataSource: DataSource,
    client: DBOSClient,
    conversations: ConversationStore,
    entries: EntryStore,
    periods: PeriodStore,
    speech: SpeechStore,
    spending: Spending,
    budget: Budget,
    schedules: SqlSchedules,
    visibility: Visibility
) extends Inbox {
  import SqlInbox.{Begun, Heard, Ingested}

  def ingest(
      origin: Origin,
      source: SourceId,
      message: Message.User,
      by: PrincipalId
  ): Either[InboxError, TurnRef] =
    inTransaction {
      conversations
        .find(origin)
        .flatMap {
          // No conversation yet: the message is new, and is refused over the cap before its
          // conversation is created.
          case None =>
            overCap(Instant.now()).flatMap {
              case Some(refused) => Right(Left(refused))
              case None =>
                recorded(
                  origin,
                  source,
                  Payload.Message(message),
                  by,
                  roomOf(origin),
                  Instant.now(),
                  capped = true
                )
            }
          case Some(_) =>
            recorded(
              origin,
              source,
              Payload.Message(message),
              by,
              roomOf(origin),
              Instant.now(),
              capped = true
            )
        }
        .flatMap {
          case Left(refused) => Right(Ingested(Left(refused), None))
          case Right(turn) => openingOf(turn).map(Ingested(Right(turn), _))
        }
    }.flatMap { ingested =>
      // Queued after the commit, and again on a redelivery: enqueued twice, it runs once.
      ingested.opening
        .fold[Either[InboxError, Unit]](Right(()))(o => enqueue(Stitches.enqueueOptions(o)))
        .flatMap(_ => ingested.turn)
    }

  def hear(
      origin: Origin,
      source: SourceId,
      text: String,
      by: PrincipalId,
      at: Instant,
      reach: Reach
  ): Either[InboxError, Unit] =
    inTransaction(
      recorded(origin, source, Payload.Heard(text), by, roomOf(origin), at, capped = false)
        .flatMap {
          case Left(_) => Right(Heard(None, None))
          case Right(turn) =>
            for {
              entry <- entries.get(InboundId.of(turn.conversationId, source))
              period <- periods.of(turn)
              triage <- entry.map(_.payload) match {
                // A message recorded as a turn before is not heard, and not triaged.
                case Some(Payload.Heard(_)) =>
                  speech
                    .heard(turn, reach)
                    .map(_ => period.map(p => TriageRef(p.ref, turn.turnSeq)))
                case _ => Right(None)
              }
              opening <- openingOf(turn)
            } yield Heard(opening, triage)
        }
    ).flatMap { heard =>
      // Enqueued after the commit, and again on a redelivery: a workflow enqueued twice runs
      // once, so a redelivery after a lost enqueue repairs it. The placement first, so a
      // room's openings queue in the order they were heard, each tried whether or not the
      // other was.
      val placed = heard.opening.fold[Either[InboxError, Unit]](Right(()))(o =>
        enqueue(Stitches.enqueueOptions(o))
      )
      val triaged = heard.triage.fold[Either[InboxError, Unit]](Right(()))(t =>
        enqueue(Triages.enqueueOptions(t))
      )
      placed.flatMap(_ => triaged)
    }

  /** `turn`'s message as an [[Opening]] of its conversation, read in the transaction open;
    * `None` when it is not one.
    */
  private def openingOf(turn: TurnRef)(using Tx^): Either[StoreError, Option[Opening]] =
    for {
      conversation <- conversations.get(turn.conversationId)
      all <- entries.list(turn.conversationId)
    } yield conversation.flatMap(Opening.of(_, all, turn))

  /** Why the day's spend at `now` refuses a new message; `None` when it does not. */
  private def overCap(now: Instant)(using Tx^): Either[StoreError, Option[InboxError]] =
    budget.cap match {
      case None => Right(None)
      case Some(cap) =>
        val day = budget.today(now)
        spending
          .on(day)
          .map(spent => Option.when(!budget.admits(spent))(InboxError.OverCap(spent, cap, day)))
    }

  /** The label a conversation an edge's message begins from `origin` is created at. */
  private def roomOf(origin: Origin): Label = visibility.roomLabel(origin.room)

  /** `payload` recorded as the first entry of a new turn, in the transaction open, dated `at`
    * as the period it opens is, its conversation created at `label` if it is new: its existing
    * turn when `source` was recorded before, else, when `capped`, refused over the cap of the
    * day `at` falls on, else a new turn.
    */
  private def recorded(
      origin: Origin,
      source: SourceId,
      payload: Payload,
      by: PrincipalId,
      label: Label,
      at: Instant,
      capped: Boolean
  )(using Tx^): Either[StoreError, Either[InboxError, TurnRef]] =
    for {
      conversation <- conversations.findOrCreate(origin, by, label)
      id = InboundId.of(conversation.id, source)
      // Serialises ingest per conversation: a concurrent ingest waits here, then sees
      // this one's entry and its sequence numbers.
      next <- entries.lockNext(conversation.id)
      existing <- entries.get(id)
      refused <- existing.fold(if (capped) overCap(at) else Right(None))(_ => Right(None))
      turn <- (existing, refused) match {
        case (Some(entry), _) => Right(Right(TurnRef(entry.conversationId, entry.turnSeq)))
        case (None, Some(why)) => Right(Left(why))
        case (None, None) =>
          periods.openFor(conversation.id, next.turnSeq, at).flatMap { _ =>
            entries
              .insert(
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
              .flatMap(_ => SqlInbox.authored(id, by))
              .map(_ => Right(TurnRef(conversation.id, next.turnSeq)))
          }
      }
    } yield turn

  def posted(
      origin: Origin,
      source: SourceId,
      text: String,
      at: Instant,
      request: CallSlot,
      by: PrincipalId
  ): Either[InboxError, Boolean] =
    inTransaction {
      conversations.find(origin).flatMap {
        case Some(_) => Right(false)
        case None =>
          for {
            conversation <- conversations.findOrCreate(origin, by, roomOf(origin))
            // Serialises with any other writer to the conversation; past the lock, what it
            // holds is settled.
            next <- entries.lockNext(conversation.id)
            before <- entries.list(conversation.id)
            recorded <-
              if (before.nonEmpty) Right(false)
              else {
                val id = InboundId.of(conversation.id, source)
                for {
                  _ <- periods.openFor(conversation.id, next.turnSeq, at)
                  _ <- entries.insert(
                    Entry(
                      id,
                      conversation.id,
                      next.turnSeq,
                      None,
                      next.seq,
                      Payload.Posted(text),
                      at
                    )
                  )
                  _ <- SqlInbox.madeBy(id, conversation.id, request)
                } yield true
              }
          } yield recorded
      }
    }

  def begun(origin: Origin): Either[InboxError, Boolean] =
    inTransaction(conversations.find(origin).map(_.nonEmpty))

  def ingested(origin: Origin, source: SourceId): Either[InboxError, Option[TurnRef]] =
    inTransaction {
      conversations.find(origin).flatMap {
        case None => Right(None)
        case Some(c) =>
          entries
            .get(InboundId.of(c.id, source))
            .map(_.collect { case e @ Entry(_, _, _, _, _, Payload.Message(_), _) =>
              TurnRef(e.conversationId, e.turnSeq)
            })
      }
    }

  def recorded(origin: Origin, sources: Set[SourceId]): Either[InboxError, Set[SourceId]] =
    inTransaction {
      conversations.find(origin).flatMap {
        case None => Right(Set.empty)
        case Some(c) =>
          sources.foldLeft[Either[StoreError, Set[SourceId]]](Right(Set.empty)) { (acc, s) =>
            acc.flatMap(found =>
              entries.get(InboundId.of(c.id, s)).map(e => if (e.isEmpty) found else found + s)
            )
          }
      }
    }

  /** From the turn's workflow status (a workflow DBOS has not heard of, queued or running is
    * Open), and, once it has ended, its reply entry. A status that cannot be read is
    * `Unavailable`, never read as the turn's end.
    */
  def progress(turn: TurnRef): Either[InboxError, Progress] = {
    val ended: Either[InboxError, Option[String]] =
      try {
        val handle = client.retrieveWorkflow[String, Exception](WorkflowId.value(turn.workflowId))
        Right(Option(handle.getStatus()).map(_.status()) match {
          case Some(WorkflowState.SUCCESS) => Some(handle.getResult())
          case Some(state) if !state.isActive() => Some(s"workflow ${state.name.toLowerCase}")
          case _ => None
        })
      } catch { case NonFatal(e) => Left(SqlInbox.unavailable(e)) }
    ended.flatMap {
      case None => Right(Progress.Open)
      case Some(outcome) =>
        inTransaction(entries.get(turn.replyId)).map { entry =>
          val reply =
            entry.map(_.payload).collect { case Payload.Message(a: Message.Assistant) => a }
          Progress.Done(reply, outcome)
        }
    }
  }

  def startTurn(turn: TurnRef): Either[InboxError, Unit] =
    inTransaction(conversations.get(turn.conversationId)).flatMap { found =>
      found.flatMap(c => Slot.of(c.origin)) match {
        case Some(slot) => Left(InboxError.SlotRun(slot))
        case None => enqueue(Turns.enqueueOptions(turn))
      }
    }

  /** Decided in one transaction under the schedule's row lock, a run's workflow status read
    * from DBOS inside it; a run it starts or restarts is enqueued after the commit, and again
    * on the next call when that enqueue was lost (DBOS then does not know the run).
    */
  def startSlot(
      schedule: ScheduleId,
      version: Option[Int],
      now: Instant
  ): Either[InboxError, Slotted] =
    inTransaction {
      schedules.held(schedule).flatMap {
        case None => Right(Begun(Slotted.Idle, None, None))
        case Some(waiting) =>
          for {
            flight <- waiting.last.flatMap((slot, running) => running.map(slot -> _)) match {
              case None => Right(None)
              case Some((slot, v)) =>
                runOf(Slot(schedule, slot), waiting.job, v).map(turn => Some(turn -> v))
            }
            state <- flight.fold[Either[StoreError, Option[InFlight]]](Right(None)) { (turn, v) =>
              inFlight(turn, v).map(Some(_))
            }
            last = waiting.last.map((slot, _) => LastRun(slot, state))
            begun <- Starting.of(waiting.rule, waiting.next, last, version, now) match {
              case Starting.Idle => Right(Begun(Slotted.Idle, None, None))
              case Starting.Start(at, following, v, superseding) =>
                val slot = Slot(schedule, at)
                for {
                  turn <- recordedRun(slot, waiting.job, waiting.label, v, now)
                  opening <- openingOf(turn)
                  _ <- schedules.started(slot, v, following)
                } yield Begun(
                  if (superseding) Slotted.Superseding(turn, slot) else Slotted.Started(turn, slot),
                  Some(turn),
                  opening
                )
              case Starting.Restart =>
                flight match {
                  case Some((turn, _)) => Right(Begun(Slotted.Restarted(turn), Some(turn), None))
                  case None => Left(StoreError.Invalid("a run restarted, with none in flight"))
                }
              case Starting.Fail =>
                last match {
                  case Some(run) =>
                    schedules
                      .failed(schedule, now)
                      .map(_ => Begun(Slotted.Failed(Slot(schedule, run.slot)), None, None))
                  case None => Left(StoreError.Invalid("a run failed, with none started"))
                }
              case Starting.Miss(at) =>
                schedules
                  .missed(schedule, now)
                  .map(_ => Begun(Slotted.Missed(Slot(schedule, at)), None, None))
              case Starting.Passed(following) =>
                last match {
                  case Some(run) =>
                    schedules
                      .passed(schedule, following, now)
                      .map(_ => Begun(Slotted.Ran(Slot(schedule, run.slot)), None, None))
                  case None => Left(StoreError.Invalid("a slot ran, with no run started"))
                }
            }
          } yield begun
      }
    }.flatMap { begun =>
      // Enqueued after the commit, and again on a later call should this be lost: a workflow
      // enqueued twice runs once. A run's opening is placed as any opening is, when its origin
      // is one placed ([[Opening.of]]).
      val placed = begun.opening.fold[Either[InboxError, Unit]](Right(()))(o =>
        enqueue(Stitches.enqueueOptions(o))
      )
      placed
        .flatMap(_ =>
          begun.run.fold[Either[InboxError, Unit]](Right(()))(t => enqueue(Runs.enqueueOptions(t)))
        )
        .map(_ => begun.slotted)
    }

  /** The turn `slot`'s run at `version` was recorded as: `job`'s run, its opening's source
    * [[Slot.source]] of its slot's conversation. Invalid when it is not recorded, which
    * [[startSlot]] never leaves a schedule in.
    */
  private def runOf(slot: Slot, job: JobName, version: Int)(using
      Tx^
  ): Either[StoreError, TurnRef] =
    conversations.find(slot.origin(job)).flatMap { found =>
      found
        .fold[Either[StoreError, Option[Entry]]](Right(None))(c =>
          entries.get(InboundId.of(c.id, Slot.source(version)))
        )
        .flatMap(
          _.map(e => TurnRef(e.conversationId, e.turnSeq))
            .toRight(StoreError.Invalid(s"the run of ${slot.key} at v$version is not recorded"))
        )
    }

  /** The state of `turn`, a run at `version` in flight: replied once its reply entry is kept;
    * otherwise by its workflow's status, `Unknown` when DBOS has no workflow under its id.
    */
  private def inFlight(turn: TurnRef, version: Int)(using Tx^): Either[StoreError, InFlight] =
    entries.get(turn.replyId).flatMap {
      case Some(_) => Right(InFlight.Replied)
      case None =>
        try
          Right(
            Option(
              client
                .retrieveWorkflow[String, Exception](WorkflowId.value(turn.workflowId))
                .getStatus()
            ).map(_.status()) match {
              case None => InFlight.Unknown
              case Some(state) if state.isActive() => InFlight.Going(version)
              case Some(_) => InFlight.Ended(version)
            }
          )
        catch {
          case NonFatal(e) =>
            Left(StoreError.DatabaseError(Option(e.getMessage).getOrElse(e.toString)))
        }
    }

  /** `slot`'s run at `version`, `job`'s, recorded in the transaction open as a new turn of its
    * conversation (created by grit at its schedule's `label`), opened at `at` by grit with
    * [[Slot.opening]]; the turn recorded before when there is one.
    */
  private def recordedRun(slot: Slot, job: JobName, label: Label, version: Int, at: Instant)(using
      Tx^
  ): Either[StoreError, TurnRef] =
    recorded(
      slot.origin(job),
      Slot.source(version),
      Payload.Message(slot.opening(job)),
      PrincipalId.Grit,
      label,
      at,
      capped = false
    ).flatMap(_.left.map(refused => StoreError.Invalid(s"a run refused: $refused")))

  /** Enqueues the workflow `options` names, with no arguments. */
  private def enqueue(options: EnqueueOptions): Either[InboxError, Unit] =
    try {
      // A repeated enqueue of the same id is a no-op (`ON CONFLICT (workflow_uuid)`). The
      // array is empty and DBOS only reads it; separation checking treats arrays as mutable.
      client.enqueueWorkflow[String, Exception](
        options,
        caps.unsafe.unsafeAssumePure(Array.empty[AnyRef])
      )
      Right(())
    } catch {
      case NonFatal(e) => Left(SqlInbox.unavailable(e))
    }

  /** `DBOSClient.send` to the turn's workflow on [[Approval.topic]], under an idempotency
    * key fixed by the workflow and the call: DBOS keeps one notification per key
    * (`ON CONFLICT (message_uuid) DO NOTHING`, NotificationsDAO.sendBulk), so a second
    * answer never reaches the turn.
    */
  def answer(
      workflow: WorkflowId,
      call: ToolCallId,
      approval: Approval
  ): Either[InboxError, Unit] =
    try {
      client.send(
        WorkflowId.value(workflow),
        Approval.encode(approval),
        Approval.topic(call),
        SqlInbox.answerKey(workflow, call)
      )
      Right(())
    } catch {
      case _: DBOSNonExistentWorkflowException => Left(InboxError.NoSuchTurn(workflow))
      case NonFatal(e) => Left(SqlInbox.unavailable(e))
    }

  /** Runs `body` in its own transaction, committing on `Right` and rolling back otherwise. */
  private def inTransaction[A](body: (Tx^) ?=> Either[StoreError, A]): Either[InboxError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        val result =
          try body(using Tx.fromConnection(conn))
          catch { case NonFatal(e) => conn.rollback(); throw e }
        result match {
          case Right(_) => conn.commit()
          case Left(_) => conn.rollback()
        }
        result.left.map {
          case StoreError.DatabaseError(cause) => InboxError.Unavailable(cause)
          case StoreError.Invalid(cause) => InboxError.Unavailable(cause)
          case StoreError.DuplicateId(id) =>
            InboxError.Unavailable(s"entry ${EntryId.value(id)} appeared mid-transaction")
        }
      }
    } catch {
      case NonFatal(e) => Left(SqlInbox.unavailable(e))
    }
}

private[dbos] object SqlInbox {

  /** What an ingest recorded: its turn, or why it was refused, and the turn's message as an
    * opening, which is placed.
    */
  private final case class Ingested(turn: Either[InboxError, TurnRef], opening: Option[Opening])

  /** What starting a slot did (`slotted`), and what it queues: the run `run`, enqueued, and its
    * opening, placed.
    */
  private final case class Begun(
      slotted: Slotted,
      run: Option[TurnRef],
      opening: Option[Opening]
  )

  /** What a hearing recorded that is queued: the heard message as an opening, and its triage. */
  private final case class Heard(opening: Option[Opening], triage: Option[TriageRef])

  /** An ingested message's entry id: deterministic, so a redelivery finds it. */
  /** Records that `by` wrote the inbound entry `id`. */
  private def authored(id: EntryId, by: PrincipalId)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    SqlEntryStore.attempt {
      Using.resource(
        conn.prepareStatement("INSERT INTO grit.inbound (entry_id, author) VALUES (?, ?)")
      ) { ps =>
        ps.setString(1, EntryId.value(id))
        ps.setString(2, PrincipalId.value(by))
        ps.executeUpdate()
        ()
      }
    }
  }

  /** Records that the hosted call at `request` made the post `id`, `conversation`'s first
    * entry.
    */
  private def madeBy(id: EntryId, conversation: ConversationId, request: CallSlot)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    SqlEntryStore.attempt {
      Using.resource(
        conn.prepareStatement(
          "INSERT INTO grit.posted (entry_id, conversation_id, request) VALUES (?, ?::uuid, ?)"
        )
      ) { ps =>
        ps.setString(1, EntryId.value(id))
        ps.setString(2, ConversationId.value(conversation))
        ps.setString(3, request.key)
        ps.executeUpdate()
        ()
      }
    }
  }

  /** The idempotency key of every answer to `call` of `workflow`: one per call. */
  def answerKey(workflow: WorkflowId, call: ToolCallId): String =
    s"answer:${WorkflowId.value(workflow)}:${ToolCallId.value(call)}"

  def unavailable(e: Throwable): InboxError.Unavailable =
    InboxError.Unavailable(Option(e.getMessage).getOrElse(e.toString))
}
