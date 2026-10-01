package grit.app.chat

import java.time.{LocalDate, ZoneOffset}

import grit.core.id.WorkflowId
import grit.core.inbox.InboxError
import grit.core.message.Cost
import grit.core.spend.{Budget, DailyCap, Day, Spend}

import utest.*

/** [[ChatHost]]'s words for a message it could not send. */
object ChatHostTests extends TestSuite {

  val tests = Tests {
    test("a message over the day's cap is told the one refusal, naming no cost; others why") {
      val cap = DailyCap.of("1").fold(e => throw new java.lang.AssertionError(e), identity)
      val day = Day(LocalDate.of(2026, 9, 28), ZoneOffset.UTC)
      ChatHost.notSent(InboxError.OverCap(Spend(3, Cost.Exact(BigDecimal("1.2"))), cap, day)) ==>
        Budget.Refusal
      ChatHost.notSent(InboxError.Unavailable("the database is down")) ==>
        "not sent: Unavailable(the database is down)"
      ChatHost.notSent(InboxError.NoSuchTurn(WorkflowId("w"))) ==> "not sent: NoSuchTurn(w)"
    }
  }
}
