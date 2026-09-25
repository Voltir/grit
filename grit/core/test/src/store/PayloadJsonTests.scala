package grit.core.store

import grit.core.id.{EntryId, ToolCallId, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.topic.{Placement, TopicEvent, TopicId, Verdict, Weights}

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
              TopicEvent.Placed(TurnSeq(2), Weights.same(TopicId("t1"), 0.5), Placement.First)
            )
          )
        )
        .render() ==>
        """{"kind":"topic","events":[{"event":"opened","topic":"t1"},""" +
        """{"event":"placed","turn":2,"weights":[["t1",0.5]],"elsewhere":0.5,"by":{"kind":"first"}}]}"""
    }

    test("exchange, attempt and ask") {
      PayloadJson.write(Payload.Exchange(samples(3))).render() ==>
        """{"kind":"exchange","message":{"role":"tool_result","callId":"c1",""" +
        """"content":"no matches","isError":true}}"""
      PayloadJson.write(Payload.Attempt(ToolCallId("c1"))).render() ==>
        """{"kind":"attempt","call":"c1"}"""
      PayloadJson.write(Payload.Ask(ToolCallId("c1"), "Run ls")).render() ==>
        """{"kind":"ask","call":"c1","shown":"Run ls"}"""
    }

    test("every sample round-trips, through text too") {
      val window = Payload.Window(Vector(EntryId("a")), Vector(TurnSeq(0), TurnSeq(7)))
      val (t1, t2) = (TopicId("topic:c:0"), TopicId("topic:c:3"))
      val topic = Payload.Topic(
        Vector(
          TopicEvent.Opened(t1),
          TopicEvent.Placed(TurnSeq(0), Weights.whole(t1), Placement.First),
          TopicEvent.Placed(TurnSeq(1), Weights.whole(t1), Placement.Unclassified("no key")),
          TopicEvent.Placed(
            TurnSeq(3),
            Weights.changed(
              t1,
              0.1,
              Vector(Placement.Chance(Some(t1), 0.2), Placement.Chance(None, 0.8)),
              Some(t2)
            ),
            Placement.Classified(
              0.1,
              Placement.Outcome.Changed(
                Vector(Placement.Chance(Some(t1), 0.2), Placement.Chance(None, 0.8))
              )
            )
          ),
          TopicEvent
            .Placed(TurnSeq(4), Weights.same(t1, 0.5), Placement.Asked(Verdict.Current, None)),
          TopicEvent.Placed(
            TurnSeq(4),
            Weights.whole(t2),
            Placement.Asked(Verdict.Earlier("Knots"), Some("called a tool again"))
          ),
          TopicEvent
            .Placed(TurnSeq(5), Weights.whole(t2), Placement.Asked(Verdict.New(None), None)),
          TopicEvent.Placed(
            TurnSeq(6),
            Weights.whole(t2),
            Placement.Asked(Verdict.New(Some("Sailing")), None)
          ),
          TopicEvent.Placed(
            TurnSeq(7),
            Weights.whole(t2),
            Placement.Asked(Verdict.Unreadable("{}"), None)
          ),
          TopicEvent.Described(t2, "Knots", "Which knot holds under load.")
        )
      )
      (samples.map(Payload.Message(_)) ++ samples.map(Payload.Exchange(_)) :+ Payload.Summary(
        "s"
      ) :+
        Payload.Query("q") :+ window :+ topic :+ Payload.Attempt(ToolCallId("c1")) :+
        Payload.Ask(ToolCallId("c1"), "Edit a.txt"))
        .foreach { p =>
          PayloadJson.read(ujson.read(PayloadJson.write(p).render())) ==> Right(p)
        }
    }

    test("a malformed payload is a Left, not a throw") {
      def placed(weights: ujson.Value, elsewhere: Double, by: ujson.Value): ujson.Value =
        ujson.Obj(
          "kind" -> "topic",
          "events" -> ujson.Arr(
            ujson.Obj(
              "event" -> "placed",
              "turn" -> 0,
              "weights" -> weights,
              "elsewhere" -> elsewhere,
              "by" -> by
            )
          )
        )
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
        placed(ujson.Arr(), 1, ujson.Obj("kind" -> "first")),
        placed(ujson.Arr(ujson.Arr("t", 0.5)), 0.4, ujson.Obj("kind" -> "first")),
        placed(
          ujson.Arr(ujson.Arr("t", 0.5), ujson.Arr("t", 0.5)),
          0,
          ujson.Obj("kind" -> "first")
        ),
        placed(ujson.Arr(ujson.Arr("t", 1.5)), -0.5, ujson.Obj("kind" -> "first")),
        placed(
          ujson.Arr(ujson.Arr("t", 1)),
          0,
          ujson.Obj(
            "kind" -> "classified",
            "pSame" -> 0.9,
            "band" -> "same",
            "choice" -> ujson.Arr(ujson.Arr(ujson.Null, 1))
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
