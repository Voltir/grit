package grit.core.store

import grit.core.id.{EntryId, ToolCallId, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.topic.{Band, Placement, TopicEvent, TopicId, Verdict}

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

    test("summary") {
      PayloadJson.write(Payload.Summary("Asked X; decided Y.")).render() ==>
        """{"kind":"summary","text":"Asked X; decided Y."}"""
    }

    test("query") {
      PayloadJson.write(Payload.Query("postgres sqlite decision")).render() ==>
        """{"kind":"query","text":"postgres sqlite decision"}"""
    }

    test("window") {
      PayloadJson
        .write(Payload.Window(Vector(EntryId("a"), EntryId("b")), Vector(TurnSeq(3))))
        .render() ==>
        """{"kind":"window","entries":["a","b"],"recalled":[3]}"""
    }

    test("topic") {
      PayloadJson
        .write(
          Payload.Topic(
            Vector(
              TopicEvent.Opened(TopicId("t1")),
              TopicEvent.Placed(TurnSeq(2), Vector(TopicId("t1") -> 0.5), 0.5, Placement.First)
            )
          )
        )
        .render() ==>
        """{"kind":"topic","events":[{"event":"opened","topic":"t1"},""" +
        """{"event":"placed","turn":2,"weights":[["t1",0.5]],"elsewhere":0.5,"by":{"kind":"first"}}]}"""
    }

    test("every sample round-trips, through text too") {
      val window = Payload.Window(Vector(EntryId("a")), Vector(TurnSeq(0), TurnSeq(7)))
      val (t1, t2) = (TopicId("topic:c:0"), TopicId("topic:c:3"))
      val topic = Payload.Topic(
        Vector(
          TopicEvent.Opened(t1),
          TopicEvent.Placed(TurnSeq(0), Vector(t1 -> 1.0), 0.0, Placement.First),
          TopicEvent.Placed(TurnSeq(1), Vector(t1 -> 1.0), 0.0, Placement.Unclassified("no key")),
          TopicEvent.Placed(
            TurnSeq(3),
            Vector(t1 -> 0.1, t2 -> 0.72),
            0.18,
            Placement.Classified(0.1, Band.Changed, Vector(Some(t1) -> 0.2, None -> 0.8))
          ),
          TopicEvent
            .Placed(TurnSeq(4), Vector(t1 -> 0.5), 0.5, Placement.Asked(Verdict.Current, None)),
          TopicEvent.Placed(
            TurnSeq(4),
            Vector(t2 -> 1.0),
            0.0,
            Placement.Asked(Verdict.Earlier("Knots"), Some("called a tool again"))
          ),
          TopicEvent
            .Placed(TurnSeq(5), Vector(t2 -> 1.0), 0.0, Placement.Asked(Verdict.New(None), None)),
          TopicEvent.Placed(
            TurnSeq(6),
            Vector(t2 -> 1.0),
            0.0,
            Placement.Asked(Verdict.New(Some("Sailing")), None)
          ),
          TopicEvent.Placed(
            TurnSeq(7),
            Vector(t2 -> 1.0),
            0.0,
            Placement.Asked(Verdict.Unreadable("{}"), None)
          ),
          TopicEvent.Described(t2, "Knots", "Which knot holds under load.")
        )
      )
      (samples.map(Payload.Message(_)) :+ Payload.Summary("s") :+ Payload.Query("q") :+ window :+
        topic)
        .foreach { p =>
          PayloadJson.read(ujson.read(PayloadJson.write(p).render())) ==> Right(p)
        }
    }

    test("a malformed payload is a Left, not a throw") {
      val bad = Seq(
        ujson.Arr(),
        ujson.Obj("kind" -> "summary"),
        ujson.Obj("kind" -> "summary", "text" -> 3),
        ujson.Obj("kind" -> "topic", "events" -> ujson.Arr(ujson.Obj("event" -> "renamed"))),
        ujson.Obj(
          "kind" -> "topic",
          "events" -> ujson.Arr(
            ujson.Obj(
              "event" -> "placed",
              "turn" -> -1,
              "weights" -> ujson.Arr(),
              "elsewhere" -> 1,
              "by" -> ujson.Obj("kind" -> "first")
            )
          )
        ),
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
