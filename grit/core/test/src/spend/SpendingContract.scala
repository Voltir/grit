package grit.core.spend

import java.time.{Instant, LocalDate, ZoneId}
import java.util.UUID

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.Cost
import grit.core.store.{Tx, UsageLedger}

import utest.*

/** The contract every [[Spending]] keeps over the ledger it reads, run against the in-memory
  * ledger in core and the SQL one in grit.dbos. The stores may be shared between tests, so
  * each test has days and conversations of its own.
  */
abstract class SpendingContract extends TestSuite {

  /** The ledger to record in, and the spending read from it. */
  protected def ledger: UsageLedger & Spending

  /** Records a call that cost `usd` (unpriced when none) for `turn` from `workflow`, as
    * held by `entry`, at `at`.
    */
  protected def recordAt(
      entry: EntryId,
      turn: TurnRef,
      workflow: WorkflowId,
      usd: Option[String],
      at: Instant
  ): Unit

  /** Runs `body` in one transaction, committed when it returns. */
  protected def transaction[A](body: (Tx^) ?=> A): A

  private val LA = ZoneId.of("America/Los_Angeles")

  private def fresh(): ConversationId = ConversationId(UUID.randomUUID().toString)

  private def call(
      c: ConversationId,
      usd: Option[String],
      at: String,
      workflow: String = "w"
  ): Unit = {
    val turn = TurnRef(c, TurnSeq.First)
    recordAt(
      EntryId(s"e:${UUID.randomUUID()}"),
      turn,
      WorkflowId(s"$workflow:${ConversationId.value(c)}"),
      usd,
      Instant.parse(at)
    )
  }

  val tests = Tests {
    test(
      "on: every conversation's calls recorded on the day as the zone's clocks show it, and none beside it"
    ) {
      val (c1, c2) = (fresh(), fresh())
      call(c1, Some("0.5"), "2031-09-10T06:59:59Z") // 23:59:59 on the 9th in Los Angeles
      call(c1, Some("0.01"), "2031-09-10T07:00:00Z") // its first instant
      call(c2, Some("0.02"), "2031-09-11T06:59:59Z") // its last second
      call(c1, Some("0.7"), "2031-09-11T07:00:00Z") // the 11th
      transaction(ledger.on(Day(LocalDate.of(2031, 9, 10), LA))) ==>
        Right(Spend(2, Cost.Exact(BigDecimal("0.03"))))
    }

    test("on: an unpriced call makes the day's cost a lower bound; a day with none is zero") {
      val c = fresh()
      call(c, Some("0.01"), "2031-09-12T12:00:00Z")
      call(c, None, "2031-09-12T13:00:00Z")
      transaction(ledger.on(Day(LocalDate.of(2031, 9, 12), LA))) ==>
        Right(Spend(2, Cost.AtLeast(BigDecimal("0.01"))))
      transaction(ledger.on(Day(LocalDate.of(2031, 1, 1), LA))) ==> Right(Spend.Zero)
    }

    test("conversation: its turns' calls and its closes', whatever the day, and no other's") {
      val (c, other) = (fresh(), fresh())
      call(c, Some("0.001"), "2031-09-13T12:00:00Z", "turn")
      call(c, Some("0.002"), "2031-09-14T12:00:00Z", "turn")
      call(c, Some("0.004"), "2031-09-15T12:00:00Z", "close")
      call(other, Some("0.1"), "2031-09-13T12:00:00Z")
      transaction(ledger.conversation(c)) ==> Right(Spend(3, Cost.Exact(BigDecimal("0.007"))))
    }
  }
}
