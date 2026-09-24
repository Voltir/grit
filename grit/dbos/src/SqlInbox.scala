package grit.dbos

import dev.dbos.transact.DBOSClient
import grit.core.{
  ConversationId,
  ConversationStore,
  Entry,
  EntryId,
  EntryStore,
  Inbox,
  InboxError,
  Message,
  Origin,
  Payload,
  SourceId,
  StoreError,
  Tx,
  TurnRef,
  TurnSeq
}
import java.time.Instant
import javax.sql.DataSource
import scala.util.Using
import scala.util.control.NonFatal

/** [[Inbox]] over Postgres alone, so an edge in another process can use it: ingest is one
  * short transaction, and a turn is started by enqueueing it through `client`.
  */
final class SqlInbox(
    dataSource: DataSource,
    client: DBOSClient,
    conversations: ConversationStore,
    entries: EntryStore
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
        next <- SqlInbox.lockNext(conversation.id)
        existing <- entries.get(id)
        turn <- existing match {
          case Some(entry) => Right(TurnRef(entry.conversationId, entry.turnSeq))
          case None =>
            val (turnSeq, seq) = next
            entries
              .insert(
                Entry(
                  id,
                  conversation.id,
                  turnSeq,
                  None,
                  seq,
                  Payload.Message(message),
                  Instant.now()
                )
              )
              .map(_ => TurnRef(conversation.id, turnSeq))
        }
      } yield turn
    }

  def startTurn(turn: TurnRef): Either[InboxError, Unit] =
    try {
      val options =
        new DBOSClient.EnqueueOptions(
          Turns.WorkflowName,
          classOf[DurableWorkflow].getName,
          Turns.QueueName
        )
          .withWorkflowId(grit.core.WorkflowId.value(turn.workflowId))
          .withQueuePartitionKey(ConversationId.value(turn.conversationId))
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

  /** Locks `conversation`'s row until the transaction ends, and returns the next turn
    * seq and entry seq after everything already recorded in it.
    */
  def lockNext(conversation: ConversationId)(using tx: Tx^): Either[StoreError, (TurnSeq, Long)] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    SqlEntryStore.attempt {
      Using.resource(
        conn.prepareStatement("SELECT 1 FROM grit.conversations WHERE id = ?::uuid FOR UPDATE")
      ) { ps =>
        ps.setString(1, ConversationId.value(conversation))
        Using.resource(ps.executeQuery())(_ => ())
      }
      Using.resource(
        conn.prepareStatement(
          """SELECT coalesce(max(turn_seq) + 1, 0) AS turn, coalesce(max(seq) + 1, 0) AS seq
            |FROM grit.entries WHERE conversation_id = ?::uuid""".stripMargin
        )
      ) { ps =>
        ps.setString(1, ConversationId.value(conversation))
        Using.resource(ps.executeQuery()) { rs =>
          rs.next()
          (TurnSeq(rs.getLong("turn")), rs.getLong("seq"))
        }
      }
    }
  }

  def unavailable(e: Throwable): InboxError.Unavailable =
    InboxError.Unavailable(Option(e.getMessage).getOrElse(e.toString))
}
