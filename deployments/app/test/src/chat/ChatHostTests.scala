package grit.app.chat

import java.time.{LocalDate, ZoneOffset}

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.inbox.InboxError
import grit.core.message.{Cost, Tokens, Usage}
import grit.core.spend.{Budget, DailyCap, Day, Spend, TurnSpend}
import grit.core.store.UsageLedger

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

    test("a turn's spend is told once it has answered, again when its rows change, and not else") {
      val c = ConversationId("c")
      def at(n: Long) = TurnRef(c, TurnSeq(n))
      def row(entry: EntryId, usd: String) =
        UsageLedger.Row(
          entry,
          "test/model",
          None,
          None,
          Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(BigDecimal(usd))),
          Tokens(1)
        )
      val replied = Vector(row(EntryId("query"), "0.001"), row(at(0).replyId, "0.002"))
      val unanswered = Vector(row(EntryId("query:1"), "0.001"))
      val first = ChatHost.spends(c, Map(TurnSeq(0) -> replied, TurnSeq(1) -> unanswered), Map.empty)
      val same = ChatHost.spends(c, Map(TurnSeq(0) -> replied), first)
      val summarised = replied :+ row(EntryId("summary"), "0.003")
      val later = ChatHost.spends(c, Map(TurnSeq(0) -> summarised), first)
      (first, same, later) ==> (
        Map(TurnSeq(0) -> TurnSpend.of(at(0), replied)),
        Map.empty,
        Map(TurnSeq(0) -> TurnSpend.of(at(0), summarised))
      )
    }
  }
}
