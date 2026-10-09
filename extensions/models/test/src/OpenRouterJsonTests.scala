package grit.models

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Effort, ReasoningReplay, Upstream}
import grit.core.provider.{ModelRequest, ProviderError, ToolSchema, ToolUse}

import utest.*

object OpenRouterJsonTests extends TestSuite {

  private def resource(name: String): ujson.Value =
    ujson.read(scala.io.Source.fromInputStream(getClass.getResourceAsStream(name)).mkString)

  private val replay = ujson.Arr(
    ujson.Obj("type" -> "reasoning.text", "text" -> "thinking", "format" -> "unknown", "index" -> 0)
  )

  private val sampleResponse = ujson.read("""{
    "id": "gen-1",
    "model": "openai/gpt-oss-20b",
    "choices": [{
      "finish_reason": "tool_calls",
      "native_finish_reason": "tool_calls",
      "message": {
        "role": "assistant",
        "content": "Let me look.",
        "reasoning": "thinking",
        "reasoning_details": [{"type": "reasoning.text", "text": "thinking", "format": "unknown", "index": 0}],
        "tool_calls": [{"id": "call_1", "type": "function",
                        "function": {"name": "read", "arguments": "{\"path\":\"a.txt\"}"}}]
      }
    }],
    "usage": {"prompt_tokens": 120, "completion_tokens": 30, "total_tokens": 150,
              "prompt_tokens_details": {"cached_tokens": 100}, "cost": 0.0000123}
  }""")

  val tests = Tests {
    test("request: the system prompt first, then each message in its role") {
      val assistant = Message.Assistant(
        Vector(
          AssistantBlock.Reasoning("thinking", Some(replay)),
          AssistantBlock.Text("Let me look."),
          AssistantBlock.ToolCall(ToolCallId("call_1"), "read", ujson.Obj("path" -> "a.txt"))
        ),
        StopReason.ToolUse,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "m"
      )
      val body = OpenRouterJson.request(
        "openai/gpt-oss-20b",
        512,
        None,
        ModelRequest(
          "be brief",
          Vector(
            Message.User("hi"),
            assistant,
            Message.ToolResult(ToolCallId("call_1"), "contents", isError = false)
          )
        )
      )
      body ==> ujson.Obj(
        "model" -> "openai/gpt-oss-20b",
        "max_tokens" -> 512,
        "messages" -> ujson.Arr(
          ujson.Obj("role" -> "system", "content" -> "be brief"),
          ujson.Obj("role" -> "user", "content" -> "hi"),
          ujson.Obj(
            "role" -> "assistant",
            "content" -> "Let me look.",
            "tool_calls" -> ujson.Arr(
              ujson.Obj(
                "id" -> "call_1",
                "type" -> "function",
                "function" -> ujson.Obj("name" -> "read", "arguments" -> """{"path":"a.txt"}""")
              )
            ),
            "reasoning_details" -> replay
          ),
          ujson.Obj("role" -> "tool", "tool_call_id" -> "call_1", "content" -> "contents")
        )
      )
    }

    test("request: tools and tool_choice only when the request has tools") {
      val topic = ToolSchema(
        "topic",
        "Say which topic.",
        ujson.Obj("type" -> "object", "properties" -> ujson.Obj())
      )
      val plain =
        OpenRouterJson.request("m", 1, None, ModelRequest("s", Vector(Message.User("hi"))))
      assert(!plain.obj.contains("tools"), !plain.obj.contains("tool_choice"))
      val auto =
        OpenRouterJson.request(
          "m",
          1,
          None,
          ModelRequest("s", Vector(Message.User("hi")), Vector(topic))
        )
      auto("tools") ==> ujson.Arr(
        ujson.Obj(
          "type" -> "function",
          "function" -> ujson.Obj(
            "name" -> "topic",
            "description" -> "Say which topic.",
            "parameters" -> ujson.Obj("type" -> "object", "properties" -> ujson.Obj())
          )
        )
      )
      auto("tool_choice") ==> ujson.Str("auto")
      val off = OpenRouterJson.request(
        "m",
        1,
        None,
        ModelRequest("s", Vector(Message.User("hi")), Vector(topic), ToolUse.Off)
      )
      off("tool_choice") ==> ujson.Str("none")
      off("tools") ==> auto("tools")
      val required = OpenRouterJson.request(
        "m",
        1,
        None,
        ModelRequest("s", Vector(Message.User("hi")), Vector(topic), ToolUse.Required)
      )
      required("tool_choice") ==> ujson.Str("required")
      required("tools") ==> auto("tools")
      val strict = OpenRouterJson.request(
        "m",
        1,
        None,
        ModelRequest("s", Vector(Message.User("hi")), Vector(topic.copy(strict = true)))
      )
      strict("tools")(0)("function")("strict") ==> ujson.True
    }

    test("request: a pinned upstream alone, with no fallbacks; open routing sends none") {
      val asked = ModelRequest("s", Vector(Message.User("hi")))
      Upstream
        .of("open-inference/fp8")
        .map(u => OpenRouterJson.request("m", 1, Some(u), asked)("provider")) ==>
        Some(ujson.Obj("order" -> ujson.Arr("open-inference/fp8"), "allow_fallbacks" -> false))
      assert(!OpenRouterJson.request("m", 1, None, asked).obj.contains("provider"))
    }

    test("request: several calls and their results keep their ids, in order") {
      val ids = Vector("a", "b", "c").map(ToolCallId(_))
      val reply = Message.Assistant(
        ids.map(id => AssistantBlock.ToolCall(id, "note", ujson.Obj("text" -> "x"))),
        StopReason.ToolUse,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "m"
      )
      val results = ids.reverse.map(id => Message.ToolResult(id, "ok", isError = false))
      val sent = OpenRouterJson
        .request("m", 1, None, ModelRequest("s", reply +: results))("messages")
        .arr
        .toVector
      sent(1)("tool_calls").arr.map(_("id").str).toVector ==> Vector("a", "b", "c")
      sent.drop(2).map(_("tool_call_id").str) ==> Vector("c", "b", "a")
    }

    test("request: effort asks for reasoning in OpenRouter's words; none asked sends none") {
      val asked = ModelRequest("s", Vector(Message.User("hi")))
      Effort.values.toVector.map(e =>
        OpenRouterJson.request("m", 1, None, asked, effort = Some(e))("reasoning")("effort").str
      ) ==> Vector("minimal", "low", "medium", "high", "xhigh", "max")
      assert(!OpenRouterJson.request("m", 1, None, asked).obj.contains("reasoning"))
    }

    test(
      "request: reasoning goes back as details, or not at all when the pair's replay is dropped"
    ) {
      val reply = OpenRouterJson.response(sampleResponse)
      def sent(r: ReasoningReplay) = reply.map(m =>
        OpenRouterJson
          .request("m", 1, None, ModelRequest("s", Vector(m)), replay = r)("messages")(1)
          .obj
      )
      sent(ReasoningReplay.Details).map(_.get("reasoning_details")) ==> Right(Some(replay))
      sent(ReasoningReplay.Dropped).map(_.get("reasoning_details")) ==> Right(None)
      sent(ReasoningReplay.Dropped).map(_.get("tool_calls").isDefined) ==> Right(true)
    }

    test("request: arguments that were not JSON go back as a JSON string of what was sent") {
      val reply = Message.Assistant(
        Vector(AssistantBlock.ToolCall(ToolCallId("c"), "f", ujson.Str("{oops"))),
        StopReason.ToolUse,
        Usage(Tokens.Zero, Tokens.Zero, Tokens.Zero, None),
        "m"
      )
      val sent = OpenRouterJson.request("m", 1, None, ModelRequest("s", Vector(reply)))
      val arguments = sent("messages")(1)("tool_calls")(0)("function")("arguments").str
      ujson.read(arguments) ==> ujson.Str("{oops")
    }

    test("response: reasoning, text and tool calls, in that order, with usage and cost") {
      OpenRouterJson.response(sampleResponse) ==> Right(
        Message.Assistant(
          Vector(
            AssistantBlock.Reasoning("thinking", Some(replay)),
            AssistantBlock.Text("Let me look."),
            AssistantBlock.ToolCall(ToolCallId("call_1"), "read", ujson.Obj("path" -> "a.txt"))
          ),
          StopReason.ToolUse,
          Usage(Tokens(120), Tokens(30), Tokens(100), Some(BigDecimal("0.0000123"))),
          "openai/gpt-oss-20b"
        )
      )
    }

    test("response: the upstream that served it is its provider, when the response names one") {
      val served = ujson.Obj.from(sampleResponse.obj)
      served("provider") = "Fireworks"
      OpenRouterJson.response(served).map(_.upstream) ==> Right(Some("Fireworks"))
      OpenRouterJson.response(sampleResponse).map(_.upstream) ==> Right(None)
    }

    test("response: finish reasons map to stop reasons, keeping unknown ones") {
      def stopOf(reason: String) = OpenRouterJson
        .response(
          ujson.Obj(
            "choices" -> ujson.Arr(
              ujson.Obj("finish_reason" -> reason, "message" -> ujson.Obj("content" -> "x"))
            )
          )
        )
        .map(_.stop)
      stopOf("stop") ==> Right(StopReason.EndTurn)
      stopOf("length") ==> Right(StopReason.MaxTokens)
      stopOf("content_filter") ==> Right(StopReason.ContentFilter)
      stopOf("weird") ==> Right(StopReason.Other("weird"))
    }

    test("response: arguments that are not JSON are kept as a string") {
      val body = ujson.read(
        """{"choices":[{"finish_reason":"tool_calls","message":{"content":null,
          |"tool_calls":[{"id":"c","type":"function","function":{"name":"f","arguments":"{oops"}}]}}]}""".stripMargin
      )
      OpenRouterJson.response(body).map(_.blocks) ==>
        Right(Vector(AssistantBlock.ToolCall(ToolCallId("c"), "f", ujson.Str("{oops"))))
    }

    test("response: a captured call whose arguments are not JSON is kept, beside the rest") {
      // openai/gpt-oss-20b @ coreweave/fp4, 2026-09-25 (the tool-decoding probe).
      val reply = OpenRouterJson.response(resource("/openrouter-nonjson-arguments.json"))
      reply.map(_.blocks.collect { case c: AssistantBlock.ToolCall => c }) ==> Right(
        Vector(
          AssistantBlock.ToolCall(
            ToolCallId("chatcmpl-tool-968bde89ce06140f"),
            "read_file",
            ujson.Str("""{"path":"src/billing/Rates.scala",""}""")
          )
        )
      )
      reply.map(_.blocks.collect { case r: AssistantBlock.Reasoning => r.text }) ==>
        Right(Vector("Check Rates.scala."))
    }

    test("response: absent or blank arguments are {}; an object sent whole is kept") {
      def argumentsOf(arguments: Option[ujson.Value]) = {
        val function = ujson.Obj("name" -> "f")
        arguments.foreach(a => function("arguments") = a)
        OpenRouterJson
          .response(
            ujson.Obj(
              "choices" -> ujson.Arr(
                ujson.Obj(
                  "message" -> ujson.Obj(
                    "tool_calls" -> ujson.Arr(ujson.Obj("id" -> "c", "function" -> function))
                  )
                )
              )
            )
          )
          .map(_.blocks.collect { case c: AssistantBlock.ToolCall => c.arguments })
      }
      argumentsOf(None) ==> Right(Vector(ujson.Obj()))
      argumentsOf(Some(ujson.Str(" "))) ==> Right(Vector(ujson.Obj()))
      argumentsOf(Some(ujson.Obj("a" -> 1))) ==> Right(Vector(ujson.Obj("a" -> 1)))
    }

    test("response: a captured top-level error in a 200 body is the model's error") {
      // Groq refusing gpt-oss-20b's leaked tool name, 2026-09-25: no choices, only the error.
      OpenRouterJson.response(resource("/openrouter-groq-rejected.json")) ==> Left(
        ProviderError.Unavailable(
          "model error: Upstream error from Groq: Tool call validation failed: tool call " +
            "validation failed: attempted to call tool 'read_file<|channel|>commentary' which " +
            "was not in request.tools (provider_unavailable)"
        )
      )
    }

    test("response: an error on the choice, or no choice at all, is a ProviderError") {
      val errored = ujson.read(
        """{"choices":[{"finish_reason":"error","message":{"content":""},
          |"error":{"code":502,"message":"upstream died"}}]}""".stripMargin
      )
      OpenRouterJson.response(errored) ==>
        Left(ProviderError.Unavailable("model error: upstream died"))
      assert(OpenRouterJson.response(ujson.Obj("choices" -> ujson.Arr())).isLeft)
    }

    test("error: the message and its error type, from the documented shape") {
      OpenRouterJson.error(
        400,
        """{"error":{"code":400,"message":"too long","metadata":{"error_type":"context_length_exceeded"}}}"""
      ) ==> ProviderError.Refused("HTTP 400: too long (context_length_exceeded)")
      OpenRouterJson.error(502, "Bad Gateway") ==> ProviderError.Unavailable(
        "HTTP 502: Bad Gateway"
      )
    }

    test("error: 408, 429 and 5xx may pass if sent again; every other status is refused") {
      val unavailable = Vector(408, 429, 500, 502, 503, 504, 529).filter { status =>
        OpenRouterJson.error(status, "{}") match {
          case ProviderError.Unavailable(_) => true
          case ProviderError.Refused(_) => false
        }
      }
      unavailable ==> Vector(408, 429, 500, 502, 503, 504, 529)
      val refused = Vector(400, 401, 402, 403, 404, 413, 422).filter { status =>
        OpenRouterJson.error(status, "{}") match {
          case ProviderError.Refused(_) => true
          case ProviderError.Unavailable(_) => false
        }
      }
      refused ==> Vector(400, 401, 402, 403, 404, 413, 422)
    }

    test("error: a body that is not JSON is its text, trimmed") {
      // Cloudflare's 504 page, as a live run met it.
      OpenRouterJson.error(504, "error code: 504\n") ==>
        ProviderError.Unavailable("HTTP 504: error code: 504")
    }

    test("modelError: a transient code may pass if sent again; any other, or none, is refused") {
      def error(fields: (String, ujson.Value)*) = ujson.Obj.from(fields).value
      OpenRouterJson.modelError(error("code" -> ujson.Num(429), "message" -> ujson.Str("slow"))) ==>
        ProviderError.Unavailable("model error: slow")
      OpenRouterJson.modelError(error("code" -> ujson.Num(400), "message" -> ujson.Str("bad"))) ==>
        ProviderError.Refused("model error: bad")
      OpenRouterJson.modelError(error("message" -> ujson.Str("odd"))) ==>
        ProviderError.Refused("model error: odd")
    }
  }
}
