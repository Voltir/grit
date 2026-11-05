package grit.eval.harness.bench

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}

import utest.*

/** [[Consumed]] read from [[Streams]]' canned run, and from small streams made for one rule. */
object ConsumedTests extends TestSuite {

  private val checkout = "/work/task"

  private def call(id: String, usage: String, content: String = "") =
    s"""{"type":"assistant","message":{"id":"$id","model":"claude-haiku-5-5","content":[$content],"usage":{$usage}},"session_id":"s"}"""

  /** An assistant message of the grit coder calling `tool` with `arguments`, as call `id`. */
  private def calling(id: String, tool: String, arguments: ujson.Value): Message =
    Message.Assistant(
      Vector(AssistantBlock.ToolCall(ToolCallId(id), tool, arguments)),
      StopReason.ToolUse,
      Usage.Zero,
      "claude-haiku-5-5"
    )

  /** What `grit work` printed: two calls' lines, the second a side call, among other lines. */
  private val gritLines: Vector[String] = Vector(
    "[provider] anthropic/claude-haiku-5-5 @ api.anthropic.com called with 1 message(s)",
    "[provider] anthropic/claude-haiku-5-5 @ api.anthropic.com answered · in 1200 · read 1000 · " +
      "write 150 · out 40 · 0.01¢",
    "[provider] anthropic/claude-haiku-5-5 @ api.anthropic.com answered · in 300 · read 0 · " +
      "write 0 · out 10 · 0.00¢",
    "[main] done"
  )

  /** A grit turn that reads a file, is refused a run, then edits; a read after the edit. */
  private def gritTurn(status: String, failed: Option[String] = None) = GritTurn(
    "c-1",
    Vector(
      Message.User("the brief"),
      calling("t1", "read", ujson.Obj("path" -> "a")),
      Message.ToolResult(ToolCallId("t1"), "héllo", isError = false),
      calling("t2", "run", ujson.Obj("argv" -> ujson.Arr("rm", "-rf", "x"))),
      Message.ToolResult(ToolCallId("t2"), "rm is not on the allowlist", isError = true),
      calling("t3", "edit", ujson.Obj("path" -> "a")),
      Message.ToolResult(ToolCallId("t3"), "edited a", isError = false),
      calling("t4", "read", ujson.Obj("path" -> "b")),
      Message.ToolResult(ToolCallId("t4"), "after the edit", isError = false)
    ),
    status,
    "the turn, rendered",
    failed
  )

  val tests = Tests {
    test(
      "each call is counted once, at its last usage, input holding the cache's reads and writes"
    ) {
      Consumed.of(Streams.at(checkout), checkout).map { c =>
        (
          c.calls.map(k => (k.id, Tokens.value(k.usage.output))),
          c.usage
        )
      } ==> Right(
        (
          Vector("msg_01a" -> 40L, "msg_01b" -> 20L, "msg_01c" -> 30L),
          Usage(Tokens(6262), Tokens(500), Tokens(4100), Some(BigDecimal("0.0031")), Tokens(2150))
        )
      )
    }

    test("before the first edit under the checkout: the calls made and the bytes tools returned") {
      Consumed
        .of(Streams.at(checkout), checkout)
        .map(c => (c.callsBeforeEdit, c.readBeforeEdit)) ==> Right((Some(2), Some(48L)))
    }

    test("an edit outside the checkout is not the first edit") {
      val lines = Vector(
        call(
          "m1",
          """"input_tokens":1,"output_tokens":1""",
          """{"type":"tool_use","id":"t1","name":"Write","input":{"file_path":"/tmp/x.scala"}}"""
        ),
        """{"type":"user","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"t1","content":"done"}]}}""",
        call(
          "m2",
          """"input_tokens":1,"output_tokens":1""",
          s"""{"type":"tool_use","id":"t2","name":"MultiEdit","input":{"file_path":"$checkout/b.scala"}}"""
        )
      )
      Consumed.of(lines, checkout).map(c => (c.callsBeforeEdit, c.readBeforeEdit)) ==>
        Right((Some(1), Some(4L)))
    }

    test("a run that edits nothing under its checkout has neither measure before an edit") {
      Consumed
        .of(Streams.at(checkout).filterNot(_.contains("toolu_03")), checkout)
        .map(c => (c.callsBeforeEdit, c.readBeforeEdit)) ==> Right((None, None))
    }

    test("each call is priced at its own prompt's rates, never at the total's") {
      // Two Claude Haiku 5.5 calls of 60,000 uncached input tokens: each under the long-context
      // card's 100,000, though together over it. $0.10 a million: $0.006 each.
      val usage = """"input_tokens":60000,"output_tokens":0"""
      Consumed.of(Vector(call("m1", usage), call("m2", usage)), checkout).map(_.usage.costUsd) ==>
        Right(Some(BigDecimal("0.012")))
    }

    test("the result gives the run's session, its reason, Claude Code's cost and each denial") {
      Consumed.of(Streams.at(checkout), checkout).map(c => (c.session, c.ended)) ==> Right(
        (
          Some("5e55-a1"),
          Some(
            Ended(
              "error_max_turns",
              error = true,
              Some(BigDecimal("0.0031")),
              Vector(Denial("Bash", """{"command":"curl example.com"}""")),
              Some(
                Usage(
                  Tokens(6262),
                  Tokens(500),
                  Tokens(4100),
                  Some(BigDecimal("0.0031")),
                  Tokens(2150)
                )
              ),
              Vector(
                "claude-haiku-5-5" -> Usage(
                  Tokens(6262),
                  Tokens(500),
                  Tokens(4100),
                  None,
                  Tokens(2150)
                )
              )
            )
          )
        )
      )
    }

    test("the init line gives the source of the key claude used") {
      Consumed.of(Streams.at(checkout), checkout).map(_.apiKeySource) ==> Right(
        Some("apiKeyHelper")
      )
    }

    test("a run killed before its result has no ending") {
      Consumed.of(Streams.at(checkout).dropRight(1), checkout).map(_.ended) ==> Right(None)
    }

    test("a result's modelUsage is summed over its models") {
      val result =
        """{"type":"result","subtype":"success","is_error":false,"session_id":"s","total_cost_usd":0.5,"modelUsage":{"claude-haiku-5-5":{"inputTokens":10,"outputTokens":3,"cacheReadInputTokens":20,"cacheCreationInputTokens":30},"claude-sonnet-5-5":{"inputTokens":1,"outputTokens":4,"cacheReadInputTokens":0,"cacheCreationInputTokens":0}}}"""
      Consumed
        .of(Vector(call("m1", """"input_tokens":1,"output_tokens":1"""), result), checkout)
        .map(_.usage) ==> Right(
        Usage(Tokens(61), Tokens(7), Tokens(20), Some(BigDecimal("0.5")), Tokens(30))
      )
    }

    test("a result with no modelUsage gives its usage") {
      val result =
        """{"type":"result","subtype":"success","is_error":false,"session_id":"s","total_cost_usd":0.25,"usage":{"input_tokens":5,"cache_creation_input_tokens":6,"cache_read_input_tokens":7,"output_tokens":77}}"""
      Consumed
        .of(Vector(call("m1", """"input_tokens":1,"output_tokens":1"""), result), checkout)
        .map(_.usage) ==> Right(
        Usage(Tokens(18), Tokens(77), Tokens(7), Some(BigDecimal("0.25")), Tokens(6))
      )
    }

    test("a model's remaining output is priced at its calls' card when they share one") {
      val rows = Vector(
        call("m1", """"input_tokens":60000,"output_tokens":10"""),
        call("m2", """"input_tokens":60000,"output_tokens":10"""),
        """{"type":"result","subtype":"success","is_error":false,"session_id":"s","total_cost_usd":0.01,"modelUsage":{"claude-haiku-5-5":{"inputTokens":120000,"outputTokens":100,"cacheReadInputTokens":0,"cacheCreationInputTokens":0}}}"""
      )
      Consumed.of(rows, checkout).map(_.gritCost) ==> Right(Right(BigDecimal("0.01205")))
    }

    test("a model whose calls straddle the long-context card says so, in place of a figure") {
      val rows = Vector(
        call("m1", """"input_tokens":60000,"output_tokens":10"""),
        call("m2", """"input_tokens":120000,"output_tokens":10"""),
        """{"type":"result","subtype":"success","is_error":false,"session_id":"s","total_cost_usd":0.1,"modelUsage":{"claude-haiku-5-5":{"inputTokens":180000,"outputTokens":100,"cacheReadInputTokens":0,"cacheCreationInputTokens":0}}}"""
      )
      Consumed.of(rows, checkout).map(_.gritCost) ==> Right(
        Left("claude-haiku-5-5's calls straddle a card")
      )
    }

    test("a side model with no calls in the events is priced and summed in, to the digit") {
      // Haiku's one call: 1,000 input at $0.10, 10 output at $0.50; its remaining 100 output at
      // $0.50. Sonnet's side calls: 500 input at $2, 50 output at $10. $0.001655 in all.
      val rows = Vector(
        call("m1", """"input_tokens":1000,"output_tokens":10"""),
        """{"type":"result","subtype":"success","is_error":false,"session_id":"s","total_cost_usd":0.002,"modelUsage":{"claude-haiku-5-5":{"inputTokens":1000,"outputTokens":110,"cacheReadInputTokens":0,"cacheCreationInputTokens":0},"claude-sonnet-5-5":{"inputTokens":500,"outputTokens":50,"cacheReadInputTokens":0,"cacheCreationInputTokens":0}}}"""
      )
      Consumed.of(rows, checkout).map(_.gritCost) ==> Right(Right(BigDecimal("0.001655")))
    }

    test("a side model past 100,000 prompt tokens on Haiku is refused, by name") {
      val rows = Vector(
        """{"type":"result","subtype":"success","is_error":false,"session_id":"s","total_cost_usd":0.1,"modelUsage":{"claude-haiku-5-5":{"inputTokens":150000,"outputTokens":10,"cacheReadInputTokens":0,"cacheCreationInputTokens":0}}}"""
      )
      Consumed.of(rows, checkout).map(_.gritCost) ==> Right(
        Left("claude-haiku-5-5's side calls may straddle a card")
      )
    }

    test(
      "a grit turn's calls are its answered lines, side calls among them, priced by grit's rates"
    ) {
      val c = Consumed.ofTurn(gritLines, gritTurn("SUCCESS"), stopped = None)
      (
        c.session,
        c.calls.map(k => (k.id, k.model, Tokens.value(k.usage.input), k.usage.costUsd)),
        c.ended.map(e => (e.byModel.map(_._1), e.claimedUsd)),
        c.gritCost
      ) ==> (
        Some("c-1"),
        Vector(
          ("call-1", "claude-haiku-5-5", 1200L, Some(BigDecimal("0.00005375"))),
          ("call-2", "claude-haiku-5-5", 300L, Some(BigDecimal("0.000035")))
        ),
        Some((Vector("claude-haiku-5-5"), None)),
        Right(BigDecimal("0.00008875"))
      )
    }

    test(
      "before a grit turn's first write or edit: the calls made and the bytes its tools returned"
    ) {
      val c = Consumed.ofTurn(gritLines, gritTurn("SUCCESS"), stopped = None)
      (c.callsBeforeEdit, c.readBeforeEdit) ==> (Some(2), Some(32L))
    }

    test("a grit turn's refused runs are its denials, and only a turn that succeeded unstopped") {
      def ending(status: String, stopped: Option[String]) =
        Consumed
          .ofTurn(gritLines, gritTurn(status), stopped)
          .ended
          .map(e => (e.reason, e.error, e.denials))
      val denied = Vector(Denial("run", """{"argv":["rm","-rf","x"]}"""))
      (
        ending("SUCCESS", None),
        ending("PENDING", None),
        ending("SUCCESS", Some("spent 10.2¢, at its cap"))
      ) ==> (
        Some(("SUCCESS", false, denied)),
        Some(("PENDING", true, denied)),
        Some(("spent 10.2¢, at its cap", true, denied))
      )
    }

    test(
      "a grit turn that failed ends in error with its failure as the reason, its workflow SUCCESS"
    ) {
      val failed = "ask: the reply to ask:5 said nothing and called no tool"
      def ending(stopped: Option[String]) =
        Consumed
          .ofTurn(gritLines, gritTurn("SUCCESS", Some(failed)), stopped)
          .ended
          .map(e => (e.reason, e.error))
      (ending(None), ending(Some("spent 10.2¢, at its cap"))) ==> (
        Some((failed, true)),
        Some(("spent 10.2¢, at its cap", true))
      )
    }

    test("a line that is not a JSON object is refused, by its number") {
      Consumed.of(Vector(Streams.run(0), "Waiting for background work"), checkout) ==>
        Left("line 2 of the run's output is not a JSON object")
    }
  }
}
