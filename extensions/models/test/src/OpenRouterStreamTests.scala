package grit.models

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens}
import grit.core.model.{Assignment, Ladder, ModelId, ModelRef, Upstream}
import grit.core.provider.{Delta, ModelRequest, ProviderError}

import utest.*

/** A streamed response folded back into the one a non-streaming call returns: a real
  * capture (`openrouter-stream.sse`, openai/gpt-oss-20b, 2026-09-24), and the ways a
  * stream goes wrong.
  */
object OpenRouterStreamTests extends TestSuite {

  private def capture: Vector[String] = resource("/openrouter-stream.sse")

  private def resource(name: String): Vector[String] =
    scala.io.Source.fromInputStream(getClass.getResourceAsStream(name)).getLines().toVector

  /** The fold of `lines`, and the deltas it told, in order. */
  private def folded(lines: Seq[String]) = {
    val told = Vector.newBuilder[Delta]
    val body = OpenRouterStream.fold(lines.iterator, d => told += d)
    (body.flatMap(OpenRouterJson.response), told.result())
  }

  private def chunk(delta: String, rest: String = ""): String =
    s"""data: {"model":"m","choices":[{"index":0,"delta":$delta$rest}]}"""

  val tests = Tests {
    test("a captured stream adds up to the message it told, piece by piece") {
      val (reply, told) = folded(capture)
      val message = reply.getOrElse(sys.error(s"no message: $reply"))
      val text = told.collect { case Delta.Text(t) => t }.mkString
      val thought = told.collect { case Delta.Reasoning(t) => t }.mkString
      message.blocks.collect { case AssistantBlock.Text(t) => t } ==> Vector(text)
      assert(text.nonEmpty, thought.nonEmpty)
      val reasoning = message.blocks.collectFirst { case r: AssistantBlock.Reasoning => r }
      reasoning.map(_.text) ==> Some(thought)
      // The details' pieces merge into one detail whose text is the whole reasoning.
      val replay = reasoning.flatMap(_.replay).flatMap(_.arrOpt).getOrElse(Seq.empty)
      replay.size ==> 1
      replay.headOption.flatMap(_.objOpt).flatMap(_.get("text")).flatMap(_.strOpt) ==>
        Some(thought)
      message.stop ==> StopReason.EndTurn
      message.model ==> "openai/gpt-oss-20b"
      // The upstream that served it, as its chunks name it.
      message.upstream ==> Some("AkashML")
      message.usage.input ==> Tokens(80)
      message.usage.output ==> Tokens(92)
      message.usage.costUsd ==> Some(BigDecimal("0.0000108"))
    }

    test("a captured tool call, beside text, finishing 'stop', is a call with whole arguments") {
      // google/gemini-2.5-flash-lite, 2026-09-24: the first piece names the call, the second
      // carries every argument; text follows in the same response.
      val (reply, told) = folded(resource("/openrouter-toolcall.sse"))
      val message = reply.getOrElse(sys.error(s"no message: $reply"))
      message.blocks.collect { case c: AssistantBlock.ToolCall => c } ==> Vector(
        AssistantBlock.ToolCall(
          grit.core.id.ToolCallId("tool_topic_YiBmMEqd5RWZsX5vhec6"),
          "topic",
          ujson.Obj("about" -> "current", "name" -> "Redis eviction")
        )
      )
      val text = told.collect { case Delta.Text(t) => t }.mkString
      assert(text.startsWith("I can help with that."))
      message.blocks.collect { case AssistantBlock.Text(t) => t } ==> Vector(text)
      message.stop ==> StopReason.EndTurn
      message.usage.costUsd ==> Some(BigDecimal("0.0000167"))
    }

    test(
      "under the seed's pin for DeepSeek v4.1 flash at fireworks, a reply's reasoning and call with no reasoning details read back, and go back on the next request at fireworks alone with an empty reasoning_content, the call answered by its result"
    ) {
      // Hand-built in the shape of openrouter-toolcall.sse, as DeepSeek at fireworks answers:
      // its reasoning as plain `reasoning` pieces, with no `reasoning_details`.
      val pin = (for {
        seed <- Seed.catalog
        id <- ModelId.of("deepseek/deepseek-v4.1-flash-20260910").toRight("no id")
        at <- Upstream.of("fireworks").toRight("no upstream")
      } yield seed.pinned(Ladder.one(Assignment(ModelRef(id, Some(at)), 8192, None))).first)
        .fold(why => throw new java.lang.AssertionError(why), identity)
      val reply = folded(resource("/openrouter-deepseek-fireworks.sse"))._1
        .fold(e => throw new java.lang.AssertionError(e.toString), identity)
      val call = reply.blocks.collectFirst { case AssistantBlock.ToolCall(id, _, _) => id }
      val next = ModelRequest(
        "s",
        Vector(
          Message.User("Read Rig.scala."),
          reply,
          Message.ToolResult(call.getOrElse(ToolCallId("")), "object Rig", isError = false)
        )
      )
      // The turn's path: the provider's configuration from the pin, its body from that.
      val c = OpenRouterConfig.of(pin)
      val body =
        OpenRouterJson.request(c.model, c.maxTokens, c.upstream, next, c.effort, c.replay, c.quirks)
      val sent = body("messages").arr.toVector
      val assistant = sent.find(_("role").str == "assistant")
      (
        reply.blocks.collect {
          case AssistantBlock.Reasoning(text, replay) => s"reasoning $text, details $replay"
          case AssistantBlock.ToolCall(_, name, args) => s"call $name ${args.render()}"
        },
        assistant.map(a =>
          (
            a.obj.get("reasoning_details"),
            a.obj.get("reasoning_content"),
            a("tool_calls").arr.map(_("id").str).toVector
          )
        ),
        sent.filter(_("role").str == "tool").map(_("tool_call_id").str),
        body("provider")
      ) ==> (
        Vector(
          "reasoning The brief names Rig.scala; read it first., details None",
          """call read {"path":"eval/harness/src/bench/Rig.scala"}"""
        ),
        Some((None, Some(ujson.Str("")), call.map(ToolCallId.value).toVector)),
        call.map(ToolCallId.value).toVector,
        ujson.Obj("order" -> ujson.Arr("fireworks"), "allow_fallbacks" -> false)
      )
      assert(call.nonEmpty)
    }

    test("tool-call pieces merge by index; two calls stay two") {
      val (reply, _) = folded(
        Vector(
          chunk(
            """{"tool_calls":[{"index":0,"id":"a","type":"function","function":{"name":"f","arguments":""}}]}"""
          ),
          chunk(
            """{"tool_calls":[{"index":1,"id":"b","type":"function","function":{"name":"g","arguments":"{}"}}]}"""
          ),
          chunk("""{"tool_calls":[{"index":0,"function":{"arguments":"{\"x\""}}]}"""),
          chunk("""{"tool_calls":[{"index":0,"function":{"arguments":":1}"}}]}"""),
          chunk("""{}""", ""","finish_reason":"tool_calls""""),
          "data: [DONE]"
        )
      )
      reply.map(_.blocks) ==> Right(
        Vector(
          AssistantBlock.ToolCall(grit.core.id.ToolCallId("a"), "f", ujson.Obj("x" -> 1)),
          AssistantBlock.ToolCall(grit.core.id.ToolCallId("b"), "g", ujson.Obj())
        )
      )
    }

    test("comments and blank lines are skipped; text alone is a message without reasoning") {
      val (reply, told) = folded(
        Vector(
          ": OPENROUTER PROCESSING",
          "",
          chunk("""{"content":"Fe"}"""),
          chunk("""{"content":"hu"}""", ""","finish_reason":"stop""""),
          "data: [DONE]"
        )
      )
      told ==> Vector(Delta.Text("Fe"), Delta.Text("hu"))
      reply.map(_.blocks) ==> Right(Vector(AssistantBlock.Text("Fehu")))
    }

    test("an error after the 200 is the call's error, refused when it has no code") {
      val (reply, _) = folded(
        Vector(
          chunk("""{"content":"Fe"}"""),
          """data: {"error":{"message":"upstream gave up"},"choices":[{"delta":{},"finish_reason":"error"}]}"""
        )
      )
      reply ==> Left(ProviderError.Refused("model error: upstream gave up"))
    }

    test("a stream cut short may pass if sent again; one not JSON is refused") {
      val (cut, _) = folded(Vector(chunk("""{"content":"Fe"}""")))
      cut ==> Left(ProviderError.Unavailable("unreadable stream: the stream ended before [DONE]"))
      val (garbled, _) = folded(Vector("data: {nope"))
      garbled ==> Left(ProviderError.Refused("unreadable stream: a chunk is not a JSON object"))
    }

    test("a captured reply of three calls: each told as it begins, each kept with its id") {
      // mistralai/mistral-small-3.2-24b-instruct @ mistral/eu, 2026-09-25 (the probe).
      val (reply, told) = folded(resource("/openrouter-toolcalls-3.sse"))
      told ==> Vector.fill(3)(Delta.Calling("note"))
      val calls = reply.map(_.blocks.collect { case c: AssistantBlock.ToolCall => c })
      calls ==> Right(
        Vector("RaqByzRpH" -> "eggs", "n4gL8tIpI" -> "flour", "eQ9HPLosw" -> "salt").map {
          (id, text) =>
            AssistantBlock.ToolCall(grit.core.id.ToolCallId(id), "note", ujson.Obj("text" -> text))
        }
      )
      reply.map(_.stop) ==> Right(StopReason.ToolUse)
    }

    test("a call is told once, when its name arrives, between the text around it") {
      val (_, told) = folded(
        Vector(
          chunk("""{"content":"Let me look."}"""),
          chunk("""{"tool_calls":[{"index":0,"id":"a","type":"function"}]}"""),
          chunk("""{"tool_calls":[{"index":0,"function":{"name":"read","arguments":""}}]}"""),
          chunk("""{"tool_calls":[{"index":0,"function":{"name":"read","arguments":"{}"}}]}"""),
          "data: [DONE]"
        )
      )
      told ==> Vector(Delta.Text("Let me look."), Delta.Calling("read"))
    }

    test("streamed arguments that are not JSON keep the call, as a string of what was sent") {
      val (reply, _) = folded(
        Vector(
          chunk(
            """{"tool_calls":[{"index":0,"id":"a","type":"function","function":{"name":"read_file","arguments":""}}]}"""
          ),
          chunk("""{"tool_calls":[{"index":0,"function":{"arguments":"{\"path\":\"src/b"}}]}"""),
          chunk(
            """{"tool_calls":[{"index":0,"function":{"arguments":"illing/Rates.scala\",\"\"}"}}]}"""
          ),
          chunk("""{}""", ""","finish_reason":"tool_calls""""),
          "data: [DONE]"
        )
      )
      reply.map(_.blocks) ==> Right(
        Vector(
          AssistantBlock.ToolCall(
            grit.core.id.ToolCallId("a"),
            "read_file",
            ujson.Str("""{"path":"src/billing/Rates.scala",""}""")
          )
        )
      )
    }
  }
}
