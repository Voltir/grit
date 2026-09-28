package grit.core.inbox

import grit.core.id.{ConversationId, EntryId, PrincipalId, SourceId, TurnRef, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.spend.Budget
import grit.core.store.Origin
import grit.dbos.sql.TestTx

import utest.*

/** The inbox contract, kept by the in-memory fake. */
object InMemoryInboxTests extends InboxContract {

  protected def withInbox[A](budget: Budget)(
      body: (Inbox, BigDecimal => Unit, Origin => Boolean) => A
  ): A = {
    val inbox = InMemoryInbox.fresh(budget)
    def spend(usd: BigDecimal): Unit = {
      val entry = EntryId(s"spent:${inbox.ledger.rows.size}")
      val turn = TurnRef(ConversationId("elsewhere"), TurnSeq.First)
      val usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, Some(usd))
      val _ = inbox.ledger.record(entry, turn, turn.workflowId, "m", usage, Tokens(1))(using
        TestTx.fake
      )
    }
    body(inbox, spend, o => inbox.conversations.all.exists(_.origin == o))
  }
}

/** What only the fake does: a turn a test ends. */
object InMemoryInboxFinishTests extends TestSuite {

  val tests = Tests {
    test("a finished turn is Done with its reply and outcome") {
      val inbox = InMemoryInbox.fresh()
      val reply: Message.Assistant = Message.Assistant(
        Vector(AssistantBlock.Text("hi")),
        StopReason.EndTurn,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "m"
      )
      val turn = inbox
        .ingest(Origin.Task("t", "r"), SourceId("m"), Message.User("x"), PrincipalId.Local)
        .fold(e => sys.error(e.toString), identity)
      inbox.finish(turn, Some(reply), "done")
      inbox.progress(turn) ==> Right(Progress.Done(Some(reply), "done"))
    }
  }
}
