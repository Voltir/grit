package grit.core.context

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, PeriodSeq, TurnSeq}
import grit.core.message.Message
import grit.core.period.{CloseReason, Closing}
import grit.core.store.{Entry, Payload}

import utest.*

object ShownTests extends TestSuite {

  private val at = Instant.parse("2026-09-20T12:00:00Z")

  private def entry(payload: Payload): Entry =
    Entry(EntryId("e"), ConversationId("c"), TurnSeq(0), None, 0, payload, at)

  val tests = Tests {
    test("a message is shown as it is; a closing entry as one user message; nothing else") {
      val closing = Closing
        .of("We talked.", None, Vector(), Vector(), Vector(), Vector())
        .getOrElse(throw new java.lang.AssertionError("closing"))
      Shown.of(entry(Payload.Message(Message.User("hi")))) ==> Some(Message.User("hi"))
      Shown.of(entry(Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, closing))) ==>
        Some(Message.User(closing.shown(at, CloseReason.Lapsed)))
      Shown.of(entry(Payload.Summary("s"))) ==> None
    }
  }
}
