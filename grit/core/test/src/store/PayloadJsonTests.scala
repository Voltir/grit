package grit.core.store

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}

import utest.*

object PayloadJsonTests extends TestSuite {

  private val assistant: Message.Assistant = Message.Assistant(
    Vector(
      AssistantBlock.Reasoning("thinking", Some(ujson.Arr(ujson.Obj("type" -> "reasoning.text")))),
      AssistantBlock.Reasoning("", None),
      AssistantBlock.Text("hi"),
      AssistantBlock.ToolCall(ToolCallId("c1"), "grep", ujson.Obj("q" -> "x"))
    ),
    StopReason.Other("weird"),
    Usage(Tokens(10), Tokens(5), Tokens(2), Some(BigDecimal("0.000123"))),
    "openai/gpt-5-mini"
  )

  private val samples: Seq[Message] = Seq(
    Message.User("hello"),
    assistant,
    assistant
      .copy(stop = StopReason.EndTurn, usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, None)),
    Message.ToolResult(ToolCallId("c1"), "no matches", isError = true)
  )

  // These pin the stored form: every entry row is written in it, so a change
  // that would make existing rows unreadable fails here first.
  val tests = Tests {
    test("user message") {
      PayloadJson.write(Payload.Message(Message.User("hello"))).render() ==>
        """{"kind":"message","message":{"role":"user","text":"hello"}}"""
    }

    test("assistant message") {
      PayloadJson.write(Payload.Message(assistant)).render() ==>
        """{"kind":"message","message":{"role":"assistant","blocks":[""" +
        """{"type":"reasoning","text":"thinking","replay":[{"type":"reasoning.text"}]},""" +
        """{"type":"reasoning","text":""},{"type":"text","text":"hi"},""" +
        """{"type":"tool_call","id":"c1","name":"grep","arguments":{"q":"x"}}],""" +
        """"usage":{"input":10,"output":5,"cachedInput":2,"costUsd":"0.000123"},""" +
        """"model":"openai/gpt-5-mini","stop":"other","stopRaw":"weird"}}"""
    }

    test("tool result") {
      PayloadJson.write(Payload.Message(samples(3))).render() ==>
        """{"kind":"message","message":{"role":"tool_result","callId":"c1",""" +
        """"content":"no matches","isError":true}}"""
    }

    test("every sample round-trips, through text too") {
      samples.foreach { m =>
        val p = Payload.Message(m)
        PayloadJson.read(ujson.read(PayloadJson.write(p).render())) ==> Right(p)
      }
    }

    test("a malformed payload is a Left, not a throw") {
      val bad = Seq(
        ujson.Arr(),
        ujson.Obj("kind" -> "summary"),
        ujson.Obj("kind" -> "message", "message" -> ujson.Obj("role" -> "user")),
        ujson.Obj(
          "kind" -> "message",
          "message" -> ujson.Obj(
            "role" -> "tool_result",
            "callId" -> "c",
            "content" -> "x",
            "isError" -> "no"
          )
        )
      )
      bad.foreach(v => assert(PayloadJson.read(v).isLeft))
    }
  }
}
