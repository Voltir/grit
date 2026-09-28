package grit.core.inbox

import grit.core.id.{PrincipalId, SourceId}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.store.Origin

import utest.*

/** The inbox contract, kept by the in-memory fake. */
object InMemoryInboxTests extends InboxContract {

  protected def withInbox[A](body: Inbox => A): A = body(InMemoryInbox.fresh())
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
