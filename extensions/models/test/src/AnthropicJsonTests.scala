package grit.models

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Effort, ReasoningReplay}
import grit.core.provider.{ModelRequest, ProviderError, ToolSchema, ToolUse}

import utest.*

object AnthropicJsonTests extends TestSuite {

  private val thinking =
    ujson.Obj("type" -> "thinking", "thinking" -> "Read it first.", "signature" -> "EqQBsig")
  private val redacted = ujson.Obj("type" -> "redacted_thinking", "data" -> "EmwKAhgB")
  private val schema = ujson.Obj("type" -> "object", "properties" -> ujson.Obj())
  private val read = ToolSchema("read", "Reads a file.", schema)
  private val usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, None)
  private val breakpoint = ujson.Obj("type" -> "ephemeral")

  private def called(blocks: AssistantBlock*): Message =
    Message.Assistant(blocks.toVector, StopReason.ToolUse, usage, "claude-haiku-5-5")

  private def sent(
      request: ModelRequest,
      effort: Option[Effort] = None,
      replay: ReasoningReplay = ReasoningReplay.Details,
      dropBlock: Boolean = false
  ): ujson.Value =
    AnthropicJson.request("claude-haiku-5-5", 1024, effort, replay, dropBlock, request)

  private val exchange = ModelRequest(
    "sys",
    Vector(
      Message.User("hi"),
      called(
        AssistantBlock.Reasoning("Read it first.", Some(thinking)),
        AssistantBlock.Text("ok"),
        AssistantBlock.ToolCall(ToolCallId("toolu_1"), "read", ujson.Obj("path" -> "a"))
      ),
      Message.ToolResult(ToolCallId("toolu_1"), "contents", isError = false),
      Message.User("note")
    ),
    Vector(read),
    stable = 3
  )

  /** The response body Anthropic documents for a call that thinks, says something and calls
    * a tool, with the prompt cache both read and written.
    */
  private val reply = ujson.read("""{
    "id": "msg_01", "type": "message", "role": "assistant", "model": "claude-haiku-5-5",
    "content": [
      {"type": "thinking", "thinking": "Read it first.", "signature": "EqQBsig"},
      {"type": "text", "text": "Checking."},
      {"type": "redacted_thinking", "data": "EmwKAhgB"},
      {"type": "tool_use", "id": "toolu_1", "name": "read", "input": {"path": "a"}}
    ],
    "stop_reason": "tool_use", "stop_sequence": null,
    "usage": {"input_tokens": 2000, "cache_creation_input_tokens": 2000,
              "cache_read_input_tokens": 6000, "output_tokens": 1000}
  }""")

  private def stopped(reason: String, details: ujson.Value = ujson.Null): ujson.Value = {
    val body = ujson.Obj.from(reply.obj)
    body("content") = ujson.Arr()
    body("stop_reason") = reason
    body("stop_details") = details
    body
  }

  val tests = Tests {
    // The bytes are pinned, key order and all: the prompt cache keys on them, so a later call
    // repeats an earlier one's prefix only when both are written alike.
    test(
      "request: tools, system and each message in its role, a breakpoint after system and after the last stable message"
    ) {
      val expected = ujson.Obj(
        "model" -> "claude-haiku-5-5",
        "max_tokens" -> 1024,
        "thinking" -> ujson.Obj("type" -> "adaptive", "display" -> "summarized"),
        "tools" -> ujson.Arr(
          ujson.Obj("name" -> "read", "description" -> "Reads a file.", "input_schema" -> schema)
        ),
        "tool_choice" -> ujson.Obj("type" -> "auto"),
        "system" -> ujson.Arr(
          ujson.Obj("type" -> "text", "text" -> "sys", "cache_control" -> breakpoint)
        ),
        "messages" -> ujson.Arr(
          ujson.Obj(
            "role" -> "user",
            "content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> "hi"))
          ),
          ujson.Obj(
            "role" -> "assistant",
            "content" -> ujson.Arr(
              thinking,
              ujson.Obj("type" -> "text", "text" -> "ok"),
              ujson.Obj(
                "type" -> "tool_use",
                "id" -> "toolu_1",
                "name" -> "read",
                "input" -> ujson.Obj("path" -> "a")
              )
            )
          ),
          ujson.Obj(
            "role" -> "user",
            "content" -> ujson.Arr(
              ujson.Obj(
                "type" -> "tool_result",
                "tool_use_id" -> "toolu_1",
                "content" -> "contents",
                "cache_control" -> breakpoint
              )
            )
          ),
          ujson.Obj(
            "role" -> "user",
            "content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> "note"))
          )
        )
      )
      ujson.write(sent(exchange)) ==> ujson.write(expected)
    }

    test("request: a failed result says so; a call whose arguments are not an object sends {}") {
      val messages = sent(
        ModelRequest(
          "s",
          Vector(
            called(AssistantBlock.ToolCall(ToolCallId("toolu_2"), "read", ujson.Str("{path"))),
            Message.ToolResult(ToolCallId("toolu_2"), "bad arguments", isError = true)
          ),
          Vector(read)
        )
      )("messages")
      messages(0)("content")(0)("input") ==> ujson.Obj()
      messages(1)("content")(0) ==> ujson.Obj(
        "type" -> "tool_result",
        "tool_use_id" -> "toolu_2",
        "content" -> "bad arguments",
        "is_error" -> true
      )
    }

    test(
      "request: a stable mark of 0 or less keeps the system breakpoint alone; past the end, the last message"
    ) {
      def marked(stable: Int): Vector[Int] = {
        val body = sent(exchange.copy(stable = stable))
        body("messages").arr.toVector.zipWithIndex.collect {
          case (m, i) if m("content").arr.exists(_.obj.contains("cache_control")) => i
        }
      }
      marked(0) ==> Vector.empty
      marked(-2) ==> Vector.empty
      marked(1) ==> Vector(0)
      marked(9) ==> Vector(3)
      sent(exchange.copy(stable = 0))("system")(0)("cache_control") ==> breakpoint
    }

    test(
      "request: the message breakpoint never sits on a thinking block; with no system, the last tool takes it"
    ) {
      val thoughtOnly = ModelRequest(
        "",
        Vector(Message.User("hi"), called(AssistantBlock.Reasoning("", Some(redacted)))),
        Vector(read, read.copy(name = "write")),
        stable = 2
      )
      val body = sent(thoughtOnly)
      body.obj.contains("system") ==> false
      body("tools").arr.map(_.obj.contains("cache_control")).toVector ==> Vector(false, true)
      body("messages")(1)("content") ==> ujson.Arr(redacted)
      body("messages")(0)("content")(0)("cache_control") ==> breakpoint
    }

    test(
      "request: each tool strict as its schema says; Required and Auto ask for auto, Off for none; no tools sends neither key"
    ) {
      def choice(use: ToolUse) =
        sent(ModelRequest("s", Vector(Message.User("hi")), Vector(read), use))("tool_choice")
      choice(ToolUse.Auto) ==> ujson.Obj("type" -> "auto")
      choice(ToolUse.Required) ==> ujson.Obj("type" -> "auto")
      choice(ToolUse.Off) ==> ujson.Obj("type" -> "none")
      val strict =
        sent(ModelRequest("s", Vector(Message.User("hi")), Vector(read.copy(strict = true))))
      strict("tools")(0)("strict") ==> ujson.True
      sent(exchange)("tools")(0).obj.contains("strict") ==> false
      val bare = sent(ModelRequest("s", Vector(Message.User("hi"))))
      (bare.obj.contains("tools"), bare.obj.contains("tool_choice")) ==> (false, false)
    }

    test(
      "request: thinking goes back verbatim in its place, not when the pair's replay is dropped, and another provider's form never"
    ) {
      def content(replay: ReasoningReplay, blocks: AssistantBlock*) =
        sent(ModelRequest("s", Vector(called(blocks*))), replay = replay)("messages")(0)("content")
      val text = ujson.Obj("type" -> "text", "text" -> "ok")
      content(
        ReasoningReplay.Details,
        AssistantBlock.Text("ok"),
        AssistantBlock.Reasoning("", Some(redacted))
      ) ==> ujson.Arr(text, redacted)
      content(
        ReasoningReplay.Dropped,
        AssistantBlock.Reasoning("Read it first.", Some(thinking)),
        AssistantBlock.Text("ok")
      ) ==> ujson.Arr(text)
      val openRouter = ujson.Arr(ujson.Obj("type" -> "reasoning.text", "text" -> "hm"))
      content(
        ReasoningReplay.Details,
        AssistantBlock.Reasoning("hm", Some(openRouter)),
        AssistantBlock.Reasoning("hm", None),
        AssistantBlock.Text("ok")
      ) ==> ujson.Arr(text)
    }

    test("request: effort in Anthropic's words under output_config; none asked sends none") {
      def effort(e: Option[Effort]) = sent(exchange, effort = e).obj.get("output_config")
      effort(Some(Effort.Minimal)) ==> Some(ujson.Obj("effort" -> "low"))
      effort(Some(Effort.XHigh)) ==> Some(ujson.Obj("effort" -> "xhigh"))
      effort(Some(Effort.Max)) ==> Some(ujson.Obj("effort" -> "max"))
      effort(None) ==> None
    }

    test("request: a mismatched thinking block is dropped only when asked") {
      sent(exchange, dropBlock = true)("thinking") ==> ujson.Obj(
        "type" -> "adaptive",
        "display" -> "summarized",
        "block_binding" -> ujson.Obj("prefix_mismatch_behavior" -> "drop_block")
      )
      sent(exchange)("thinking").obj.contains("block_binding") ==> false
    }

    test(
      "response: its blocks in order, thinking kept verbatim, input counting reads and writes, cost, served by api.anthropic.com"
    ) {
      AnthropicJson.response(reply) ==> Right(
        Message.Assistant(
          Vector(
            AssistantBlock.Reasoning("Read it first.", Some(thinking)),
            AssistantBlock.Text("Checking."),
            AssistantBlock.Reasoning("", Some(redacted)),
            AssistantBlock.ToolCall(ToolCallId("toolu_1"), "read", ujson.Obj("path" -> "a"))
          ),
          StopReason.ToolUse,
          Usage(
            Tokens(10000),
            Tokens(1000),
            Tokens(6000),
            Some(BigDecimal("0.00101")),
            Tokens(2000)
          ),
          "claude-haiku-5-5",
          Some("api.anthropic.com")
        )
      )
    }

    test("response: stop reasons map, keeping unknown ones") {
      def stop(reason: String) = AnthropicJson.response(stopped(reason)).map(_.stop)
      stop("end_turn") ==> Right(StopReason.EndTurn)
      stop("tool_use") ==> Right(StopReason.ToolUse)
      stop("max_tokens") ==> Right(StopReason.MaxTokens)
      stop("pause_turn") ==> Right(StopReason.Other("pause_turn"))
      stop("model_context_window_exceeded") ==>
        Right(StopReason.Other("model_context_window_exceeded"))
    }

    test("response: a refusal is Refused naming its category, or saying it has none") {
      AnthropicJson.response(
        stopped("refusal", ujson.Obj("type" -> "refusal", "category" -> "cyber"))
      ) ==> Left(ProviderError.Refused("the model declined to answer (category: cyber)"))
      AnthropicJson.response(
        stopped("refusal", ujson.Obj("type" -> "refusal", "category" -> ujson.Null))
      ) ==> Left(ProviderError.Refused("the model declined to answer (no category)"))
      AnthropicJson.response(stopped("refusal")) ==>
        Left(ProviderError.Refused("the model declined to answer (no category)"))
    }

    test("error: the message and its error type; 408, 429 and 5xx may pass if sent again") {
      val overloaded =
        """{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}"""
      AnthropicJson.error(529, overloaded) ==>
        ProviderError.Unavailable("HTTP 529: Overloaded (overloaded_error)")
      val invalid =
        """{"type":"error","error":{"type":"invalid_request_error","message":"bad"}}"""
      AnthropicJson.error(400, invalid) ==>
        ProviderError.Refused("HTTP 400: bad (invalid_request_error)")
      AnthropicJson.error(502, "Bad Gateway\n") ==> ProviderError.Unavailable(
        "HTTP 502: Bad Gateway"
      )
    }
  }
}
