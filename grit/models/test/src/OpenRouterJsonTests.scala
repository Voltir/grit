package grit.models

import grit.core.*

import utest.*

object OpenRouterJsonTests extends TestSuite {

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

    test("response: a reply read back and sent again is the same message") {
      val resent = OpenRouterJson.response(sampleResponse).map { reply =>
        OpenRouterJson.request("m", 1, ModelRequest("s", Vector(reply)))("messages")(1)
      }
      resent.map(_("reasoning_details")) ==> Right(replay)
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
      ) ==> ProviderError.Unavailable("HTTP 400: too long (context_length_exceeded)")
      OpenRouterJson.error(502, "Bad Gateway") ==> ProviderError.Unavailable(
        "HTTP 502: Bad Gateway"
      )
    }

    test("config: the key is required, the model defaults, and toString hides the key") {
      OpenRouterConfig.fromEnv(Map.empty) ==>
        Left(OpenRouterConfig.Invalid.Missing("OPENROUTER_API_KEY"))
      OpenRouterConfig.fromEnv(Map("OPENROUTER_API_KEY" -> " ")) ==>
        Left(OpenRouterConfig.Invalid.Empty("OPENROUTER_API_KEY"))
      val config = OpenRouterConfig.fromEnv(Map("OPENROUTER_API_KEY" -> "sk-or-secret"))
      config.map(_.model) ==> Right(OpenRouterConfig.DefaultModel)
      assert(!config.toString.contains("sk-or-secret"))
      OpenRouterConfig
        .fromEnv(Map("OPENROUTER_API_KEY" -> "k", "GRIT_MODEL" -> "x/y"))
        .map(_.model) ==>
        Right("x/y")
    }
  }
}
