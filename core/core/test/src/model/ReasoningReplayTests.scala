package grit.core.model

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Usage}

import utest.*

object ReasoningReplayTests extends TestSuite {

  private def reply(text: String): Message.Assistant =
    Message.Assistant(
      Vector(AssistantBlock.Reasoning("hm", None), AssistantBlock.Text(text)),
      StopReason.ToolUse,
      Usage.Zero,
      "m"
    )

  private val loop: Vector[Message] = Vector(
    Message.User("go"),
    reply("first"),
    Message.ToolResult(ToolCallId("call_1"), "ok", isError = false),
    reply("second"),
    Message.ToolResult(ToolCallId("call_2"), "ok", isError = false),
    Message.User("a note")
  )

  private val everySent: Message.Assistant -> Boolean = _ => true

  val tests = Tests {

    test("details sends every message's reasoning, and dropped none") {
      ReasoningReplay.Details.sends(loop, everySent) ==> Vector.fill(6)(true)
      ReasoningReplay.Dropped.sends(loop, everySent) ==> Vector.fill(6)(false)
    }

    test("latest sends the last sent reply's alone, wherever it sits among results and notes") {
      ReasoningReplay.Latest.sends(loop, everySent) ==>
        Vector(false, false, false, true, false, false)
    }

    test("latest passes over a reply the wire would not send, to the one before it") {
      val unsent: Message.Assistant => Boolean = a => AssistantBlock.text(a.blocks) != "second"
      ReasoningReplay.Latest.sends(loop, unsent) ==>
        Vector(false, true, false, false, false, false)
    }

    test("latest sends none in a request with no reply") {
      ReasoningReplay.Latest.sends(Vector(Message.User("go")), everySent) ==> Vector(false)
    }
  }
}
