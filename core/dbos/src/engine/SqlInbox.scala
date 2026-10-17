package grit.dbos.engine

import java.time.Instant
import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.approval.Approval
import grit.core.clock.Clock
import grit.core.id.{
  CallSlot,
  ConversationId,
  EntryId,
  JobName,
  ScheduleId,
  SourceId,
  ToolCallId,
  TurnRef,
  WorkflowId
}
import grit.core.identity.Account
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
import grit.core.visibility.{Integrity, Label, Visibility}
import grit.dbos.sql.{SqlSchedules, SqlStatements}
import grit.dbos.workflow.{Runs, Stitches, Triages, Turns}

import dev.dbos.transact.exceptions.DBOSNonExistentWorkflowException
import dev.dbos.transact.workflow.WorkflowState
import dev.dbos.transact.{DBOSClient, EnqueueOptions}

/** [[Inbox]] over Postgres alone, so an edge in another process can use it: ingest is one
  * short transaction, which refuses a new message once `spending` says today's spend has
  * reached `budget`'s cap (today by `clock`, in `budget`'s zone), an ingested message dated by
  * `clock`, and a turn is started by enqueueing it through `client`. A conversation it creates for an edge's origin is
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
    visibility: Visibility,
    clock: Clock^
) extends Inbox {
  import SqlInbox.Begun

  private val opener = new grit.dbos.sql.Opener(visibility)

  private val recorder = new Recorder(conversations, entries, periods, speech, spending, budget)

  def ingest(
      origin: Origin,
      source: SourceId,
      message: Message.User,
      by: Account
  ): Either[InboxError, TurnRef] =
    SqlInbox.writtenBy(origin, by).flatMap(_ => ingested(origin, source, message, by))

  private def ingested(
      origin: Origin,
      source: SourceId,
      message: Message.User,
      by: Account
  ): Either[InboxError, TurnRef] = {
    val now = clock.now()
    inTransaction(recorder.said(origin, source, message, by, now)).flatMap {
      case Left(refused) => Left(refused)
      case Right(said) =>
        // Queued after the commit, and again on a redelivery: enqueued twice, it runs once.
        said.opening
          .fold[Either[InboxError, Unit]](Right(()))(o => enqueue(Stitches.enqueueOptions(o)))
          .map(_ => said.turn)
    }
  }

  def hear(
      origin: Origin,
      source: SourceId,
      text: String,
      by: Account,
      at: Instant,
      reach: Reach
  ): Either[InboxError, Unit] =
    SqlInbox
      .undirected(origin, "a direct message is never heard")
      .flatMap(_ => heard(origin, source, text, by, at, reach))

  private def heard(
      origin: Origin,
      source: SourceId,
      text: String,
      by: Account,
      at: Instant,
      reach: Reach
  ): Either[InboxError, Unit] =
    inTransaction(recorder.heard(origin, source, text, by, at, reach)).flatMap { heard =>
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

  def overCap(): Either[InboxError, Option[InboxError.OverCap]] = {
    val now = clock.now()
    inTransaction(recorder.overCapAt(now))
  }

  def posted(
      origin: Origin,
      source: SourceId,
      text: String,
      at: Instant,
      request: CallSlot,
      by: Account
  ): Either[InboxError, Boolean] =
    SqlInbox
      .undirected(origin, "grit's posts begin no direct message")
      .flatMap(_ => postedAs(origin, source, text, at, request, by))

  private def postedAs(
      origin: Origin,
      source: SourceId,
      text: String,
      at: Instant,
      request: CallSlot,
      by: Account
  ): Either[InboxError, Boolean] =
    inTransaction {
      conversations.find(origin).flatMap {
        case Some(_) => Right(false)
        case None =>
          for {
            label <- recorder.roomOf(origin)
            conversation <- conversations.findOrCreate(origin, by, label)
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
                  // grit's words, believed as the turn that posted them was then.
                  posting <- entries.believed(request.turn)
                  _ <- entries.insert(
                    Entry(
                      id,
                      conversation.id,
                      next.turnSeq,
                      None,
                      next.seq,
                      Payload.Posted(text),
                      at,
                      posting
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
            .map(_.collect { case e @ Entry(_, _, _, _, _, Payload.Message(_), _, _) =>
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
                  turn <- recordedRun(slot, waiting.job, waiting.label, waiting.integrity, v, now)
                  opening <- recorder.openingOf(turn)
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
    * [[Slot.opening]], believed at its schedule's `integrity`; the turn recorded before when
    * there is one.
    */
  private def recordedRun(
      slot: Slot,
      job: JobName,
      label: Label,
      integrity: Integrity,
      version: Int,
      at: Instant
  )(using Tx^): Either[StoreError, TurnRef] =
    recorder.recorded(
      slot.origin(job),
      Slot.source(version),
      Payload.Message(slot.opening(job)),
      Account.Grit,
      label,
      Some(integrity),
      at,
      capped = false
    ).flatMap(_.left.map(refused => StoreError.Invalid(s"a run refused: $refused")))

  /** Enqueues the workflow `options` names, with no arguments. */
  private def enqueue(options: EnqueueOptions): Either[InboxError, Unit] =
    try {
      // A repeated enqueue of the same id is a no-op (`ON CONFLICT (workflow_uuid)`).
      client.enqueueWorkflow[String, Exception](
        options,
        // The array is empty and DBOS only reads it; separation checking treats arrays as mutable.
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

  /** Runs `body` in its own transaction at maintenance's clearance, committing on `Right` and
    * rolling back otherwise.
    */
  private def inTransaction[A](body: (Tx^) ?=> Either[StoreError, A]): Either[InboxError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        val result =
          try opener.maintained(conn).flatMap(tx => body(using tx))
          catch { case NonFatal(e) => conn.rollback(); throw e }
        result match {
          case Right(_) => conn.commit()
          case Left(_) => conn.rollback()
        }
        result.left.map(InboxError.stored)
      }
    } catch {
      case NonFatal(e) => Left(SqlInbox.unavailable(e))
    }
}

private[dbos] object SqlInbox {

  /** What starting a slot did (`slotted`), and what it queues: the run `run`, enqueued, and its
    * opening, placed.
    */
  private final case class Begun(
      slotted: Slotted,
      run: Option[TurnRef],
      opening: Option[Opening]
  )

  /** Records that the hosted call at `request` made the post `id`, `conversation`'s first
    * entry.
    */
  private def madeBy(id: EntryId, conversation: ConversationId, request: CallSlot)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    SqlStatements.attempt {
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

  /** `Invalid` for a direct message written through an account not its own: a person writes
    * only their own.
    */
  def writtenBy(origin: Origin, by: Account): Either[InboxError, Unit] =
    origin match {
      case Origin.Direct(account, _) if (account: Account) != by =>
        Left(
          InboxError.Invalid(
            s"a direct message with ${Account.written(account)} written through ${Account.written(by)}"
          )
        )
      case _ => Right(())
    }

  /** `Invalid`, saying `why`, for a direct message. */
  def undirected(origin: Origin, why: String): Either[InboxError, Unit] =
    origin match {
      case Origin.Direct(_, _) => Left(InboxError.Invalid(why))
      case Origin.Tui(_, _) | Origin.Slack(_, _, _) | Origin.Task(_, _) => Right(())
    }

  def unavailable(e: Throwable): InboxError.Unavailable =
    InboxError.Unavailable(Option(e.getMessage).getOrElse(e.toString))
}
