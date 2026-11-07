package grit.models

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Effort, ReasoningReplay}
import grit.core.provider.{ModelRequest, ProviderError, Repeats, ToolSchema, ToolUse}

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
      effort: Effort = Effort.Medium,
      replay: ReasoningReplay = ReasoningReplay.Details,
      dropBlock: Boolean = false
  ): ujson.Value =
    AnthropicJson.request("claude-haiku-5-5", 1024, effort, replay, dropBlock, request)

  /** Where `body` is marked: "system", "tool <i>", or "<message index>:<block index>". */
  private def marksIn(body: ujson.Value): Vector[String] = {
    def marked(v: ujson.Value): Boolean = v.obj.contains("cache_control")
    body.obj.get("system").fold(Vector.empty)(_.arr.toVector.filter(marked).map(_ => "system")) ++
      body.obj
        .get("tools")
        .fold(Vector.empty)(_.arr.toVector.zipWithIndex.collect {
          case (t, i) if marked(t) => s"tool $i"
        }) ++
      body("messages").arr.toVector.zipWithIndex.flatMap((m, i) =>
        m("content").arrOpt.fold(Vector.empty)(_.toVector.zipWithIndex.collect {
          case (b, j) if marked(b) => s"$i:$j"
        })
      )
  }

  /** A boundary's request: its brief, two drawn messages (the reply's thinking last), the dated
    * line, then the turn's own message.
    */
  private val boundary = ModelRequest(
    "sys",
    Vector(
      Message.User("the brief"),
      Message.User("earlier"),
      called(AssistantBlock.Text("ok"), AssistantBlock.Reasoning("Read it.", Some(thinking))),
      Message.User("dated"),
      Message.User("now")
    ),
    Vector(read),
    repeats = Repeats.of(1, 3, 5)
  )

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
    repeats = Repeats.stable(3)
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
    test(
      "request: a breakpoint after system and after the task's, the established and the stable messages, never on thinking, a block two counts share marked once"
    ) {
      val shared = boundary.copy(repeats = Repeats.of(3, 3, 3))
      Vector(marksIn(sent(boundary)), marksIn(sent(shared))) ==> Vector(
        Vector("system", "0:0", "2:0", "4:0"),
        Vector("system", "2:0")
      )
    }

    // The bytes are pinned, key order and all: the prompt cache keys on them, so a later call
    // repeats an earlier one's prefix only when both are written alike.
    test(
      "request: tools, system and each message in its role, a breakpoint after system and after the last stable message"
    ) {
      val expected = ujson.Obj(
        "model" -> "claude-haiku-5-5",
        "max_tokens" -> 1024,
        "thinking" -> ujson.Obj("type" -> "adaptive", "display" -> "summarized"),
        "output_config" -> ujson.Obj("effort" -> "medium"),
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
        val body = sent(exchange.copy(repeats = Repeats.stable(stable)))
        body("messages").arr.toVector.zipWithIndex.collect {
          case (m, i) if m("content").arr.exists(_.obj.contains("cache_control")) => i
        }
      }
      marked(0) ==> Vector.empty
      marked(-2) ==> Vector.empty
      marked(1) ==> Vector(0)
      marked(9) ==> Vector(3)
      sent(exchange.copy(repeats = Repeats.stable(0)))("system")(0)("cache_control") ==> breakpoint
    }

    test(
      "prefix: tools, system, then each message's role and blocks through the last breakpoint, a line each, unmarked"
    ) {
      def text(t: String, marked: Boolean = false) = {
        val block = ujson.Obj("type" -> "text", "text" -> t)
        if (marked) block("cache_control") = breakpoint
        block
      }
      val body = ujson.Obj(
        "model" -> "claude-haiku-5-5",
        "tools" -> ujson.Arr(ujson.Obj("name" -> "read", "strict" -> true)),
        "system" -> ujson.Arr(text("sys", marked = true)),
        "messages" -> ujson.Arr(
          ujson.Obj("role" -> "user", "content" -> ujson.Arr(text("hi"), text("there", true))),
          ujson.Obj("role" -> "assistant", "content" -> ujson.Arr(text("tail")))
        )
      )
      AnthropicJson.prefix(body) ==>
        """{"name":"read","strict":true}
          |{"type":"text","text":"sys"}
          |{"role":"user"}
          |{"type":"text","text":"hi"}
          |{"type":"text","text":"there"}
          |""".stripMargin
      body("system")(0).obj.remove("cache_control")
      body("messages")(0)("content")(1).obj.remove("cache_control")
      AnthropicJson.prefix(body) ==> ""
    }

    test(
      "request: the message breakpoint never sits on a thinking block; with no system, the last tool takes it"
    ) {
      val thoughtOnly = ModelRequest(
        "",
        Vector(Message.User("hi"), called(AssistantBlock.Reasoning("", Some(redacted)))),
        Vector(read, read.copy(name = "write")),
        repeats = Repeats.stable(2)
      )
      val body = sent(thoughtOnly)
      body.obj.contains("system") ==> false
      body("tools").arr.map(_.obj.contains("cache_control")).toVector ==> Vector(false, true)
      body("messages")(1)("content") ==> ujson.Arr(redacted)
      body("messages")(0)("content")(0)("cache_control") ==> breakpoint
    }

    test(
      "request: each tool strict as its schema says; Required and Auto ask for auto (its thinking refuses a forced choice); no tools sends neither key"
    ) {
      def choice(use: ToolUse) =
        sent(ModelRequest("s", Vector(Message.User("hi")), Vector(read), use))("tool_choice")
      choice(ToolUse.Auto) ==> ujson.Obj("type" -> "auto")
      choice(ToolUse.Required) ==> ujson.Obj("type" -> "auto")
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

    test("request: effort in Anthropic's words under output_config") {
      def effort(e: Effort) = sent(exchange, effort = e).obj.get("output_config")
      effort(Effort.Minimal) ==> Some(ujson.Obj("effort" -> "low"))
      effort(Effort.XHigh) ==> Some(ujson.Obj("effort" -> "xhigh"))
      effort(Effort.Max) ==> Some(ujson.Obj("effort" -> "max"))
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

    test(
      "response: thinking is usage's output_tokens_details.thinking_tokens, and not reported without it"
    ) {
      val detailed = ujson.copy(reply)
      detailed("usage")("output_tokens_details") = ujson.Obj("thinking_tokens" -> 312)
      def thinking(body: ujson.Value) = AnthropicJson.response(body).map(_.usage.thinking)
      (thinking(detailed), thinking(reply)) ==> (Right(Some(Tokens(312))), Right(None))
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

    test("error: an error object without a message says so, with its type when it has one") {
      (
        AnthropicJson.error(400, """{"error":{"type":"invalid_request_error"}}"""),
        AnthropicJson.error(400, """{"error":{}}""")
      ) ==> (
        ProviderError.Refused("HTTP 400: no message (invalid_request_error)"),
        ProviderError.Refused("HTTP 400: no message")
      )
    }
  }
}
