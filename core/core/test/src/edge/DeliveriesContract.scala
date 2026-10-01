package grit.core.edge

import grit.core.id.{ConversationId, TurnRef, TurnSeq, WorkflowId}
import grit.core.store.{StoreError, Tx}

import utest.*

/** The contract every [[Deliveries]] keeps, run against the in-memory fake in core and the SQL
  * store in grit.dbos. Each test is given a store of its own.
  */
abstract class DeliveriesContract extends TestSuite {

  /** A store with nothing awaited, and a conversation turns may be taken from. */
  protected def fresh(): (Deliveries, ConversationId)

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

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
  }
}
