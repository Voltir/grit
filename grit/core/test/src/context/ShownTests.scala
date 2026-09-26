package grit.core.context

import java.time.Instant

import grit.core.id.{ConversationId, EntryId, PeriodSeq, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, TestClosings}
import grit.core.place.Place
import grit.core.store.{Entry, Payload}

import utest.*

object ShownTests extends TestSuite {

  private val at = Instant.parse("2026-09-20T12:00:00Z")

  private def entry(payload: Payload): Entry =
    Entry(EntryId("e"), ConversationId("c"), TurnSeq(0), None, 0, payload, at)

  val tests = Tests {
    test("a message is shown as it is; a closing entry as one user message; nothing else") {
      val closing = TestClosings.prose("We talked.")
      Shown.of(entry(Payload.Message(Message.User("hi")))) ==> Some(Message.User("hi"))
      Shown.of(entry(Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, closing))) ==>
        Some(Message.User(closing.shown(at, CloseReason.Lapsed)))
      Shown.of(entry(Payload.Summary("s"))) ==> None
    }

    test("a nearby section is one user message: from where, then each message's text as a line") {
      val api = Place.read("fs:/home/nick/api").fold(e => sys.error(e), identity)
      val reply = Message.Assistant(
        Vector(AssistantBlock.Reasoning("hm", None), AssistantBlock.Text("Pin TZ=UTC.")),
        StopReason.EndTurn,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "m"
      )
      Shown.nearby(
        api,
        Vector(
          entry(Payload.Message(Message.User("flaky test"))),
          entry(Payload.Summary("a summary")),
          entry(Payload.Message(reply))
        )
      ) ==> Some(
        Message.User(
          "From another conversation of yours, still open, at fs:/home/nick/api:\n" +
            "User: flaky test\nAssistant: Pin TZ=UTC."
        )
      )
      Shown.nearby(api, Vector(entry(Payload.Summary("only a summary")))) ==> None
    }
  }
}
