package grit.core.store

import grit.core.id.{ConversationId, DocumentVersion, EntrySeq, PeriodSeq, ToolCallId, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, Closing, Probability, TestClosings}
import grit.core.place.Place
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

  private val closing: Closing = TestClosings.prose("Small talk.", Some("none"))

  private val samples: Seq[Message] = Seq(
    Message.User("hello"),
    assistant,
    assistant
      .copy(stop = StopReason.EndTurn, usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, None)),
    Message.ToolResult(ToolCallId("c1"), "no matches", isError = true),
    assistant.copy(upstream = Some("Fireworks"))
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

    test("assistant message with the upstream that served it") {
      PayloadJson.write(Payload.Message(assistant.copy(upstream = Some("Fireworks")))).render() ==>
        """{"kind":"message","message":{"role":"assistant","blocks":[""" +
        """{"type":"reasoning","text":"thinking","replay":[{"type":"reasoning.text"}]},""" +
        """{"type":"reasoning","text":""},{"type":"text","text":"hi"},""" +
        """{"type":"tool_call","id":"c1","name":"grep","arguments":{"q":"x"}}],""" +
        """"usage":{"input":10,"output":5,"cachedInput":2,"costUsd":"0.000123"},""" +
        """"model":"openai/gpt-5-mini","upstream":"Fireworks","stop":"other","stopRaw":"weird"}}"""
    }

    test("tool result") {
      PayloadJson.write(Payload.Message(samples(3))).render() ==>
        """{"kind":"message","message":{"role":"tool_result","callId":"c1",""" +
        """"content":"no matches","isError":true}}"""
    }

    test("heard message") {
      val heard = Payload.Heard("standup moves to 10:00")
      val stored = """{"kind":"heard","text":"standup moves to 10:00"}"""
      PayloadJson.write(heard).render() ==> stored
      PayloadJson.read(ujson.read(stored)) ==> Right(heard)
    }

    test("a post a conversation begins with is stored under its own kind") {
      val posted = Payload.Posted("The engine's open issues.")
      val stored = """{"kind":"posted","text":"The engine's open issues."}"""
      PayloadJson.write(posted).render() ==> stored
      PayloadJson.read(ujson.read(stored)) ==> Right(posted)
    }

    test("draft: an assistant's message under its own kind, and nothing else read as one") {
      val reply: Message.Assistant = Message.Assistant(
        Vector(AssistantBlock.Text("the freeze moved to Thursday")),
        StopReason.EndTurn,
        Usage(Tokens(3), Tokens(2), Tokens.Zero, None),
        "m"
      )
      val stored =
        """{"kind":"draft","message":{"role":"assistant","blocks":[""" +
          """{"type":"text","text":"the freeze moved to Thursday"}],""" +
          """"usage":{"input":3,"output":2,"cachedInput":0},"model":"m","stop":"end_turn"}}"""
      PayloadJson.write(Payload.Draft(reply)).render() ==> stored
      PayloadJson.read(ujson.read(stored)) ==> Right(Payload.Draft(reply))
      PayloadJson.read(
        ujson.read("""{"kind":"draft","message":{"role":"user","text":"hi"}}""")
      ) ==> Left("a draft holds an assistant's message")
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
      // Stored data: a window without sections or documents is written byte for byte as it
      // was before either existed, and every window written then reads back.
      val written = """{"kind":"window","entries":[0,1],"recalled":[3]}"""
      val window = Payload.Window(Vector(EntrySeq(0), EntrySeq(1)), Vector(TurnSeq(3)))
      PayloadJson.write(window).render() ==> written
      PayloadJson.read(ujson.read(written)) ==> Right(window)
    }

    test("a window with nearby sections keeps them under their place, as written") {
      // Stored data: a window without sections keeps the form above, byte for byte.
      val api = Place.read("fs:/home/nick/api").fold(e => sys.error(e), identity)
      val window = Payload.Window(
        Vector(EntrySeq(0)),
        Vector.empty,
        Vector(Nearby.Open(ConversationId("c9"), api, Vector(EntrySeq(5), EntrySeq(6))))
      )
      PayloadJson.write(window).render() ==>
        """{"kind":"window","entries":[0],"recalled":[],""" +
        """"nearby":[{"conversation":"c9","place":"fs:/home/nick/api","entries":[5,6]}]}"""
      PayloadJson.read(ujson.read(PayloadJson.write(window).render())) ==> Right(window)
    }

    test("a window's documents are stored by version after its sections, and read back") {
      val api = Place.read("fs:/home/nick/api").fold(e => sys.error(e), identity)
      val versions = Vector(7L, 3L).flatMap(DocumentVersion.of)
      val window = Payload.Window(
        Vector(EntrySeq(0)),
        Vector.empty,
        Vector(Nearby.Open(ConversationId("c9"), api, Vector(EntrySeq(5)))),
        versions
      )
      PayloadJson.write(window).render() ==>
        """{"kind":"window","entries":[0],"recalled":[],""" +
        """"nearby":[{"conversation":"c9","place":"fs:/home/nick/api","entries":[5]}],""" +
        """"documents":[7,3]}"""
      PayloadJson.read(ujson.read(PayloadJson.write(window).render())) ==> Right(window)
      val bare = Payload.Window(Vector(EntrySeq(0)), Vector.empty, Vector.empty, versions)
      PayloadJson.write(bare).render() ==>
        """{"kind":"window","entries":[0],"recalled":[],"documents":[7,3]}"""
      PayloadJson
        .read(ujson.read("""{"kind":"window","entries":[0],"recalled":[],"documents":[0]}"""))
        .isLeft ==> true
    }

    test("a closed nearby section is stored with its closing, and read back") {
      // Stored data: an open section keeps the form above; a closed one names its closing.
      val thread = Place.read("slack:T1/C1/1.0").fold(e => sys.error(e), identity)
      val window = Payload.Window(
        Vector(EntrySeq(0)),
        Vector.empty,
        Vector(Nearby.Closed(ConversationId("c9"), thread, EntrySeq(8)))
      )
      PayloadJson.write(window).render() ==>
        """{"kind":"window","entries":[0],"recalled":[],""" +
        """"nearby":[{"conversation":"c9","place":"slack:T1/C1/1.0","closing":8}]}"""
      PayloadJson.read(ujson.read(PayloadJson.write(window).render())) ==> Right(window)
    }

    test("a strand section is stored marked as one, and read back; an unmarked one is open") {
      val thread = Place.read("slack:T/C1/1.0").fold(e => sys.error(e), identity)
      val along: Payload.Window = Payload.Window(
        Vector(EntrySeq(0)),
        Vector.empty,
        Vector(Nearby.Along(ConversationId("c9"), thread, Vector(EntrySeq(5), EntrySeq(6))))
      )
      val json = PayloadJson.write(along)
      // The stored mark: windows recorded with a strand must keep reading as one.
      json("nearby")(0)("strand") ==> ujson.True
      PayloadJson.read(json) ==> Right(along)
      json("nearby")(0).obj.remove("strand")
      PayloadJson.read(json) ==> Right(
        along.copy(nearby =
          Vector(Nearby.Open(ConversationId("c9"), thread, Vector(EntrySeq(5), EntrySeq(6))))
        )
      )
    }

    test("an asked section is stored marked as one, and read back as one") {
      val thread = Place.read("slack:T/C2/3.0").fold(e => sys.error(e), identity)
      val asked: Payload.Window = Payload.Window(
        Vector(EntrySeq(0)),
        Vector.empty,
        Vector(Nearby.Asked(ConversationId("c9"), thread, Vector(EntrySeq(5), EntrySeq(6))))
      )
      val json = PayloadJson.write(asked)
      json("nearby")(0)("asked") ==> ujson.True
      PayloadJson.read(json) ==> Right(asked)
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

    test("exchange, result, attempt and ask") {
      val result: Message.ToolResult =
        Message.ToolResult(ToolCallId("c1"), "no matches", isError = true)
      // The message inside, as a plain message's; only the kind differs.
      PayloadJson.write(Payload.Exchange(assistant))("message") ==>
        PayloadJson.write(Payload.Message(assistant))("message")
      PayloadJson.write(Payload.Exchange(assistant))("kind") ==> ujson.Str("exchange")
      PayloadJson.write(Payload.Exchange(assistant)).obj.keySet ==> Set("kind", "message")
      PayloadJson.write(Payload.Result(result, "search \"x\" .")).render() ==>
        """{"kind":"exchange","message":{"role":"tool_result","callId":"c1",""" +
        """"content":"no matches","isError":true},"shown":"search \"x\" ."}"""
      // A result kept before calls were shown reads, its call's id in the call's place.
      PayloadJson.read(
        ujson.read(
          """{"kind":"exchange","message":{"role":"tool_result","callId":"c1",""" +
            """"content":"no matches","isError":true}}"""
        )
      ) ==> Right(Payload.Result(result, "c1"))
      PayloadJson.read(
        ujson.Obj("kind" -> "exchange", "message" -> ujson.Obj("role" -> "user", "text" -> "hi"))
      ) ==> Left("an exchange holds no user message")
      PayloadJson.write(Payload.Attempt(ToolCallId("c1"))).render() ==>
        """{"kind":"attempt","call":"c1"}"""
      PayloadJson.write(Payload.Ask(ToolCallId("c1"), "Run ls")).render() ==>
        """{"kind":"ask","call":"c1","shown":"Run ls"}"""
    }

    test("closed") {
      PayloadJson.write(Payload.Closed(PeriodSeq.First, CloseReason.Lapsed, closing)).render() ==>
        """{"kind":"closed","period":1,"reason":"lapsed","closing":""" +
        """{"v":3,"flows":{"prose":"Small talk.","outcome":"none","changes":[]},"balance":{"open":[],"standing":[],"topics":[]}}}"""
      // A pin of the stored form: a resolved close keeps its confidence beside its reason, and
      // a closing entry outlives every raw entry of its period.
      val resolved = CloseReason.Resolved(
        Probability.of(0.86).getOrElse(throw new java.lang.AssertionError("p"))
      )
      PayloadJson.write(Payload.Closed(PeriodSeq.First, resolved, closing)).render() ==>
        """{"kind":"closed","period":1,"reason":"resolved","confidence":0.86,"closing":""" +
        """{"v":3,"flows":{"prose":"Small talk.","outcome":"none","changes":[]},"balance":{"open":[],"standing":[],"topics":[]}}}"""
      PayloadJson.read(
        ujson.read(
          """{"kind":"closed","period":1,"reason":"resolved","closing":{"v":2,"flows":{"prose":"x"}}}"""
        )
      ) ==> Left("a resolved close has no confidence")
      // An unearned close, as a period only heard and kept nothing of closes (ADR 0020).
      val unearned = Payload.Closed(PeriodSeq.First, CloseReason.Unearned, closing)
      PayloadJson.write(unearned).render() ==>
        """{"kind":"closed","period":1,"reason":"unearned","closing":""" +
        """{"v":3,"flows":{"prose":"Small talk.","outcome":"none","changes":[]},"balance":{"open":[],"standing":[],"topics":[]}}}"""
      PayloadJson.read(PayloadJson.write(unearned)) ==> Right(unearned)
      PayloadJson.readReason("unearned", Some(0.5)) ==> Left("an unearned close has a confidence")
      PayloadJson.read(
        ujson.read("""{"kind":"closed","period":0,"reason":"lapsed","closing":{}}""")
      ) ==>
        Left("period 0 is below the first")
      PayloadJson.read(
        ujson.read(
          """{"kind":"closed","period":1,"reason":"quit","closing":{"v":2,"flows":{"prose":"x"}}}"""
        )
      ) ==> Left("unknown close reason: quit")
    }

    test("every sample round-trips, through text too") {
      val window = Payload.Window(Vector(EntrySeq(0)), Vector(TurnSeq(0), TurnSeq(7)))
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
      (samples.map(Payload.Message(_)) ++ samples.collect { case a: Message.Assistant =>
        Payload.Exchange(a)
      } ++ samples.collect { case r: Message.ToolResult => Payload.Result(r, "read a.txt") } :+
        Payload.Summary("s") :+
        Payload.Query("q") :+ window :+ topic :+ Payload.Attempt(ToolCallId("c1")) :+
        Payload.Ask(ToolCallId("c1"), "Edit a.txt") :+
        Payload.Closed(
          PeriodSeq.First,
          CloseReason.Resolved(
            Probability.of(0.86).getOrElse(throw new java.lang.AssertionError("p"))
          ),
          closing
        ))
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
