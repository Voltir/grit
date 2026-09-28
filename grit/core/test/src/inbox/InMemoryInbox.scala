package grit.core.inbox

import grit.core.approval.Approval
import grit.core.id.{EntryId, PrincipalId, SourceId, ToolCallId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.store.{
  Entry,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryPrincipals,
  Origin,
  Payload,
  Tx
}
import grit.dbos.sql.TestTx

/** An in-memory [[Inbox]] for tests, keeping [[InboxContract]], over the in-memory stores it
  * is given: an ingested message is an entry of its conversation, its author told to
  * `principals`. No turn runs: a test ends one with [[finish]].
  */
final class InMemoryInbox(
    val conversations: InMemoryConversationStore,
    val entries: InMemoryEntryStore,
    val principals: InMemoryPrincipals
) extends Inbox {

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

  def ingest(
      origin: Origin,
      source: SourceId,
      message: Message.User,
      by: PrincipalId
  ): Either[InboxError, TurnRef] =
    if (down) unavailable
    else
      inTx {
        val result = for {
          conversation <- conversations.findOrCreate(origin, by)
          id = InMemoryInbox.entryId(conversation.id.toString, source)
          existing <- entries.get(id)
          turn <- existing match {
            case Some(e) => Right(TurnRef(e.conversationId, e.turnSeq))
            case None =>
              entries.lockNext(conversation.id).flatMap { next =>
                entries
                  .insert(
                    Entry(
                      id,
                      conversation.id,
                      next.turnSeq,
                      None,
                      next.seq,
                      Payload.Message(message),
                      java.time.Instant.EPOCH
                    )
                  )
                  .map { _ =>
                    principals.authored(id, by)
                    TurnRef(conversation.id, next.turnSeq)
                  }
              }
          }
        } yield turn
        result.left.map(e => InboxError.Unavailable(e.toString))
      }

  def ingested(origin: Origin, source: SourceId): Either[InboxError, Option[TurnRef]] =
    if (down) unavailable
    else
      inTx {
        conversations.all.find(_.origin == origin) match {
          case None => Right(None)
          case Some(c) =>
            entries
              .get(InMemoryInbox.entryId(c.id.toString, source))
              .map(_.map(e => TurnRef(e.conversationId, e.turnSeq)))
              .left
              .map(e => InboxError.Unavailable(e.toString))
        }
      }

  def progress(turn: TurnRef): Either[InboxError, Progress] =
    if (down) unavailable else Right(finished.getOrElse(turn, Progress.Open))

  def startTurn(turn: TurnRef): Either[InboxError, Unit] =
    if (down) unavailable
    else {
      if (!started.contains(turn)) started = started :+ turn
      Right(())
    }

  def answer(workflow: WorkflowId, call: ToolCallId, approval: Approval): Either[InboxError, Unit] =
    if (down) unavailable
    else {
      answers = answers :+ ((workflow, call, approval))
      Right(())
    }
}

object InMemoryInbox {

  /** Empty in-memory stores. */
  def fresh(): InMemoryInbox =
    new InMemoryInbox(new InMemoryConversationStore, new InMemoryEntryStore, new InMemoryPrincipals)

  private def entryId(conversation: String, source: SourceId): EntryId =
    EntryId(s"in:$conversation:${SourceId.value(source)}")
}
