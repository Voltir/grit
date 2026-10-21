package grit.core.edge

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, ToolCallId, TurnRef, TurnSeq, WorkflowId}
import grit.core.identity.{Account, TestAccounts}
import grit.core.message.Message
import grit.core.store.{Payload, StoreError, Tx}
import grit.core.visibility.{Clearance, Label, Level, TestLabels}

import utest.*

/** The contract every [[Deliveries]] keeps, run against the in-memory fake in core and the SQL
  * store in grit.dbos. Each test is given a store of its own.
  */
abstract class DeliveriesContract extends TestSuite {

  /** A store with nothing awaited, and a conversation turns may be taken from. */
  protected def fresh(): (Deliveries, ConversationId)

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  /** A store with nothing awaited, and a turn begun by a message `by` wrote, in a conversation
    * kept at [[DeliveriesContract.Kept]].
    */
  protected def asking(by: Account): DeliveriesContract.Asking

  private val ana = TestAccounts.account("slack:T1/U-ana")

  private val later = Instant.parse("2999-01-01T00:00:00Z")

  private val reads = Clearance.of(DeliveriesContract.Kept)

  private def turn(c: ConversationId, n: Long): TurnRef = TurnRef(c, TurnSeq(n))

  val tests = Tests {
    test("an awaited turn is pending, with where it goes, until delivered") {
      val (deliveries, c) = fresh()
      transaction(deliveries.await(turn(c, 0), "C1/1.0")) ==> Right(())
      transaction(deliveries.pending()) ==> Right(Vector(Pending(turn(c, 0), "C1/1.0", Map.empty)))
      transaction(deliveries.delivered(turn(c, 0))) ==> Right(())
      transaction(deliveries.pending()) ==> Right(Vector.empty)
    }

    test("pending turns come in the order they were awaited") {
      val (deliveries, c) = fresh()
      transaction(deliveries.await(turn(c, 1), "b")) ==> Right(())
      transaction(deliveries.await(turn(c, 0), "a")) ==> Right(())
      transaction(deliveries.pending()).map(_.map(_.turn)) ==> Right(Vector(turn(c, 1), turn(c, 0)))
    }

    test("awaiting again changes nothing, and a delivered turn is never pending again") {
      val (deliveries, c) = fresh()
      transaction(deliveries.await(turn(c, 0), "first")) ==> Right(())
      transaction(deliveries.await(turn(c, 0), "second")) ==> Right(())
      transaction(deliveries.pending()).map(_.map(_.to)) ==> Right(Vector("first"))
      transaction(deliveries.delivered(turn(c, 0))) ==> Right(())
      transaction(deliveries.await(turn(c, 0), "third")) ==> Right(())
      transaction(deliveries.pending()) ==> Right(Vector.empty)
    }

    test("each part is posting, then posted as its id") {
      val (deliveries, c) = fresh()
      val t = turn(c, 0)
      transaction(deliveries.await(t, "to")) ==> Right(())
      transaction(deliveries.posting(t, 0)) ==> Right(())
      transaction(deliveries.pending()).map(_.map(_.parts)) ==> Right(
        Vector(Map(0 -> Part.Posting))
      )
      transaction(deliveries.posted(t, 0, "1.1")) ==> Right(())
      transaction(deliveries.posting(t, 1)) ==> Right(())
      transaction(deliveries.pending()).map(_.map(_.parts)) ==>
        Right(Vector(Map(0 -> Part.Posted("1.1"), 1 -> Part.Posting)))
    }

    test("a turn never awaited cannot be posted or delivered") {
      val (deliveries, c) = fresh()
      val t = turn(c, 7)
      val why =
        Left(StoreError.Invalid(s"no delivery awaited for ${WorkflowId.value(t.workflowId)}"))
      transaction(deliveries.posting(t, 0)) ==> why
      transaction(deliveries.posted(t, 0, "x")) ==> why
      transaction(deliveries.delivered(t)) ==> why
    }

    test(
      "a turn's open asks are listed oldest first, each with its first entry's account and until; a settled or expired one is not listed"
    ) {
      val a = asking(ana)
      val (c1, c2, c3, c4, c5) =
        (ToolCallId("c1"), ToolCallId("c2"), ToolCallId("c3"), ToolCallId("c4"), ToolCallId("c5"))
      a.append(EntryId("ask:c1"), Payload.Ask(c1, "Edit a.txt", Some(later)))
      a.append(EntryId("ask:c2"), Payload.Ask(c2, "Edit b.txt", Some(later.plusSeconds(60))))
      a.append(EntryId("ask:c3"), Payload.Ask(c3, "Edit c.txt", Some(Instant.EPOCH)))
      a.append(EntryId("ask:c4"), Payload.Ask(c4, "Edit d.txt", None))
      a.append(EntryId("ask:c5"), Payload.Ask(c5, "Edit e.txt", Some(later)))
      a.append(
        EntryId("result:c5"),
        Payload.Result(Message.ToolResult(c5, "edited", false), "edit e.txt")
      )
      a.at(reads)(a.deliveries.asks(a.turn)) ==> Right(
        Vector(
          OpenAsk(c1, "Edit a.txt", ana, later),
          OpenAsk(c2, "Edit b.txt", ana, later.plusSeconds(60))
        )
      )
    }

    test(
      "read under a public transaction, a turn's asks are none; under its own subject, they are listed"
    ) {
      val a = asking(ana)
      a.append(EntryId("ask:c1"), Payload.Ask(ToolCallId("c1"), "Edit a.txt", Some(later)))
      (
        a.at(Clearance.of(Label.Public))(a.deliveries.asks(a.turn)),
        a.at(reads)(a.deliveries.asks(a.turn))
      ) ==> (
        Right(Vector.empty),
        Right(Vector(OpenAsk(ToolCallId("c1"), "Edit a.txt", ana, later)))
      )
    }
  }
}

object DeliveriesContract {

  /** The label [[DeliveriesContract.asking]]'s conversation is kept at: above public, so a
    * transaction cleared only for public reads none of its entries.
    */
  val Kept: Label = Label.at(Level.Confidential, TestLabels.trial)

  /** A turn to ask from: `deliveries` over it, `turn` itself; `append` adds an entry holding a
    * payload to it, after everything in its conversation, as the turn would write it; `at` runs
    * a transaction at a clearance.
    */
  trait Asking {
    def deliveries: Deliveries
    def turn: TurnRef
    def append(id: EntryId, payload: Payload): Unit
    def at[A](clearance: Clearance)(body: (Tx^) ?=> A): A
  }
}
