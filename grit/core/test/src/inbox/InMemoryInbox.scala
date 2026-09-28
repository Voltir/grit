package grit.core.inbox

import grit.core.approval.Approval
import grit.core.id.{EntryId, PrincipalId, SourceId, ToolCallId, TurnRef, WorkflowId}
import grit.core.message.Message
import grit.core.spend.Budget
import grit.core.store.{
  Entry,
  InMemoryConversationStore,
  InMemoryEntryStore,
  InMemoryPrincipals,
  InMemoryUsageLedger,
  Origin,
  Payload,
  StoreError,
  Tx
}
import grit.dbos.sql.TestTx

/** An in-memory [[Inbox]] for tests, keeping [[InboxContract]], over the in-memory stores it
  * is given: an ingested message is an entry of its conversation, its author told to
  * `principals`, and a new one is refused once `ledger`'s spend today reaches `budget`'s cap,
  * today being the day `ledger.now` falls on. No turn runs: a test ends one with [[finish]].
  */
final class InMemoryInbox(
    val conversations: InMemoryConversationStore,
    val entries: InMemoryEntryStore,
    val principals: InMemoryPrincipals,
    val ledger: InMemoryUsageLedger,
    budget: Budget
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
        val known = conversations.all.find(_.origin == origin)
        val result: Either[StoreError, Either[InboxError, TurnRef]] = for {
          before <- known.fold(Right(None): Either[StoreError, Option[Entry]])(c =>
            entries.get(InMemoryInbox.entryId(c.id.toString, source))
          )
          refused <- before.fold(overCap)(_ => Right(None))
          turn <- (before, refused) match {
            case (Some(e), _) => Right(Right(TurnRef(e.conversationId, e.turnSeq)))
            case (None, Some(why)) => Right(Left(why))
            case (None, None) =>
              for {
                conversation <- conversations.findOrCreate(origin, by)
                id = InMemoryInbox.entryId(conversation.id.toString, source)
                next <- entries.lockNext(conversation.id)
                _ <- entries.insert(
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
              } yield {
                principals.authored(id, by)
                Right(TurnRef(conversation.id, next.turnSeq))
              }
          }
        } yield turn
        result.left.map(e => InboxError.Unavailable(e.toString)).flatMap(identity)
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
  def fresh(budget: Budget = Budget(java.time.ZoneOffset.UTC, None)): InMemoryInbox =
    new InMemoryInbox(
      new InMemoryConversationStore,
      new InMemoryEntryStore,
      new InMemoryPrincipals,
      new InMemoryUsageLedger,
      budget
    )

  private def entryId(conversation: String, source: SourceId): EntryId =
    EntryId(s"in:$conversation:${SourceId.value(source)}")
}
