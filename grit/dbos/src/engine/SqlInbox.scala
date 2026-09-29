package grit.dbos.engine

import java.time.Instant
import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.approval.Approval
import grit.core.id.{
  ConversationId,
  EntryId,
  PrincipalId,
  SourceId,
  ToolCallId,
  TriageRef,
  TurnRef,
  WorkflowId
}
import grit.core.inbox.{Inbox, InboxError, Progress}
import grit.core.message.Message
import grit.core.spend.{Budget, Spending}
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
import grit.dbos.sql.SqlEntryStore
import grit.dbos.workflow.{Triages, Turns}

import dev.dbos.transact.DBOSClient
import dev.dbos.transact.exceptions.DBOSNonExistentWorkflowException
import dev.dbos.transact.workflow.WorkflowState

/** [[Inbox]] over Postgres alone, so an edge in another process can use it: ingest is one
  * short transaction, which refuses a new message once `spending` says today's spend has
  * reached `budget`'s cap (today by this machine's clock, in `budget`'s zone), and a turn is
  * started by enqueueing it through `client`.
  */
final class SqlInbox(
    dataSource: DataSource,
    client: DBOSClient,
    conversations: ConversationStore,
    entries: EntryStore,
    periods: PeriodStore,
    spending: Spending,
    budget: Budget
) extends Inbox {

  def ingest(
      origin: Origin,
      source: SourceId,
      message: Message.User,
      by: PrincipalId
  ): Either[InboxError, TurnRef] =
    inTransaction {
      conversations.find(origin).flatMap {
        // No conversation yet: the message is new, and is refused over the cap before its
        // conversation is created.
        case None =>
          overCap(Instant.now()).flatMap {
            case Some(refused) => Right(Left(refused))
            case None => recorded(origin, source, Payload.Message(message), by, capped = true)
          }
        case Some(_) => recorded(origin, source, Payload.Message(message), by, capped = true)
      }
    }.flatMap(identity)

  def hear(
      origin: Origin,
      source: SourceId,
      text: String,
      by: PrincipalId
  ): Either[InboxError, Unit] =
    inTransaction(
      recorded(origin, source, Payload.Heard(text), by, capped = false).flatMap {
        case Left(_) => Right(None)
        case Right(turn) =>
          for {
            entry <- entries.get(SqlInbox.entryId(turn.conversationId, source))
            period <- periods.of(turn)
          } yield entry.map(_.payload) match {
            // A message recorded as a turn before is not heard, and not triaged.
            case Some(Payload.Heard(_)) => period.map(p => TriageRef(p.ref, turn.turnSeq))
            case _ => None
          }
      }
    ).flatMap {
      case None => Right(())
      // Enqueued after the commit, and again on a redelivery: a triage enqueued twice runs
      // once, so a redelivery after a lost enqueue repairs it.
      case Some(triage) => enqueue(Triages.enqueueOptions(triage))
    }

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

  /** `payload` recorded as the first entry of a new turn, in the transaction open: its
    * existing turn when `source` was recorded before, else, when `capped`, refused over the
    * cap, else a new turn.
    */
  private def recorded(
      origin: Origin,
      source: SourceId,
      payload: Payload,
      by: PrincipalId,
      capped: Boolean
  )(using Tx^): Either[StoreError, Either[InboxError, TurnRef]] =
    for {
      conversation <- conversations.findOrCreate(origin, by)
      id = SqlInbox.entryId(conversation.id, source)
      // Serialises ingest per conversation: a concurrent ingest waits here, then sees
      // this one's entry and its sequence numbers.
      next <- entries.lockNext(conversation.id)
      existing <- entries.get(id)
      at = Instant.now()
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

  def ingested(origin: Origin, source: SourceId): Either[InboxError, Option[TurnRef]] =
    inTransaction {
      conversations.find(origin).flatMap {
        case None => Right(None)
        case Some(c) =>
          entries
            .get(SqlInbox.entryId(c.id, source))
            .map(_.collect { case e @ Entry(_, _, _, _, _, Payload.Message(_), _) =>
              TurnRef(e.conversationId, e.turnSeq)
            })
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
    enqueue(Turns.enqueueOptions(turn))

  /** Enqueues the workflow `options` names, with no arguments. */
  private def enqueue(options: DBOSClient.EnqueueOptions): Either[InboxError, Unit] =
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

  def entryId(conversation: ConversationId, source: SourceId): EntryId =
    EntryId(s"in:${ConversationId.value(conversation)}:${SourceId.value(source)}")

  /** The idempotency key of every answer to `call` of `workflow`: one per call. */
  def answerKey(workflow: WorkflowId, call: ToolCallId): String =
    s"answer:${WorkflowId.value(workflow)}:${ToolCallId.value(call)}"

  def unavailable(e: Throwable): InboxError.Unavailable =
    InboxError.Unavailable(Option(e.getMessage).getOrElse(e.toString))
}
