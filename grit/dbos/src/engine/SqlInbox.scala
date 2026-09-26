package grit.dbos.engine

import java.time.Instant
import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.approval.Approval
import grit.core.id.{ConversationId, EntryId, SourceId, ToolCallId, TurnRef, WorkflowId}
import grit.core.inbox.{Inbox, InboxError}
import grit.core.message.Message
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
import grit.dbos.workflow.Turns

import dev.dbos.transact.DBOSClient
import dev.dbos.transact.exceptions.DBOSNonExistentWorkflowException

/** [[Inbox]] over Postgres alone, so an edge in another process can use it: ingest is one
  * short transaction, and a turn is started by enqueueing it through `client`.
  */
final class SqlInbox(
    dataSource: DataSource,
    client: DBOSClient,
    conversations: ConversationStore,
    entries: EntryStore,
    periods: PeriodStore
) extends Inbox {

  def ingest(
      origin: Origin,
      source: SourceId,
      message: Message.User
  ): Either[InboxError, TurnRef] =
    inTransaction {
      for {
        conversation <- conversations.findOrCreate(origin)
        id = SqlInbox.entryId(conversation.id, source)
        // Serialises ingest per conversation: a concurrent ingest waits here, then sees
        // this one's entry and its sequence numbers.
        next <- entries.lockNext(conversation.id)
        existing <- entries.get(id)
        turn <- existing match {
          case Some(entry) => Right(TurnRef(entry.conversationId, entry.turnSeq))
          case None =>
            val at = Instant.now()
            periods.openFor(conversation.id, next.turnSeq, at).flatMap { _ =>
              entries
                .insert(
                  Entry(
                    id,
                    conversation.id,
                    next.turnSeq,
                    None,
                    next.seq,
                    Payload.Message(message),
                    at
                  )
                )
                .map(_ => TurnRef(conversation.id, next.turnSeq))
            }
        }
      } yield turn
    }

  def startTurn(turn: TurnRef): Either[InboxError, Unit] =
    try {
      // A repeated enqueue of the same id is a no-op (`ON CONFLICT (workflow_uuid)`). The
      // array is empty and DBOS only reads it; separation checking treats arrays as mutable.
      client.enqueueWorkflow[String, Exception](
        Turns.enqueueOptions(turn),
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
  def entryId(conversation: ConversationId, source: SourceId): EntryId =
    EntryId(s"in:${ConversationId.value(conversation)}:${SourceId.value(source)}")

  /** The idempotency key of every answer to `call` of `workflow`: one per call. */
  def answerKey(workflow: WorkflowId, call: ToolCallId): String =
    s"answer:${WorkflowId.value(workflow)}:${ToolCallId.value(call)}"

  def unavailable(e: Throwable): InboxError.Unavailable =
    InboxError.Unavailable(Option(e.getMessage).getOrElse(e.toString))
}
