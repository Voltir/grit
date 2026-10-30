package grit.models

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{Delta, ProviderError}

import utest.*

/** A streamed response folded back into the one a plain call returns. `anthropic-stream.sse`
  * is written by hand from the event shapes Anthropic documents ("Streaming messages"): a
  * thinking block, text, a redacted thinking block and a tool call, a ping between them, and
  * usage cumulative in `message_delta`.
  */
object AnthropicStreamTests extends TestSuite {

  private def resource(name: String): Vector[String] =
    scala.io.Source.fromInputStream(getClass.getResourceAsStream(name)).getLines().toVector

  /** The fold of `lines` read as a response, and the deltas it told, in order. */
  private def folded(lines: Seq[String]) = {
    val told = Vector.newBuilder[Delta]
    val body = AnthropicStream.fold(lines.iterator, d => told += d)
    (body.flatMap(AnthropicJson.response), told.result())
  }

  private def event(data: String): Vector[String] = Vector(s"data: $data", "")

  private val started: Vector[String] = event(
    """{"type":"message_start","message":{"model":"claude-haiku-5-5","content":[],"usage":{"input_tokens":5,"output_tokens":1}}}"""
  )

  val tests = Tests {
    test("a stream adds up to the response a plain call returns, telling each piece as it comes") {
      val (message, told) = folded(resource("/anthropic-stream.sse"))
      message ==> Right(
        Message.Assistant(
          Vector(
            AssistantBlock.Reasoning(
              "Read it first.",
              Some(
                ujson.Obj(
                  "type" -> "thinking",
                  "thinking" -> "Read it first.",
                  "signature" -> "EqQBsig"
                )
              )
            ),
            AssistantBlock.Text("Checking."),
            AssistantBlock.Reasoning(
              "",
              Some(ujson.Obj("type" -> "redacted_thinking", "data" -> "EmwKAhgB"))
            ),
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
      told ==> Vector(
        Delta.Reasoning("Read it "),
        Delta.Reasoning("first."),
        Delta.Text("Check"),
        Delta.Text("ing."),
        Delta.Calling("read")
      )
    }

    test("an overloaded or api error event may pass if sent again; any other is refused") {
      def failed(kind: String) = folded(
        started ++ event(s"""{"type":"error","error":{"type":"$kind","message":"stopped"}}""")
      )._1
      failed("overloaded_error") ==>
        Left(ProviderError.Unavailable("model error: stopped (overloaded_error)"))
      failed("api_error") ==> Left(ProviderError.Unavailable("model error: stopped (api_error)"))
      failed("invalid_request_error") ==>
        Left(ProviderError.Refused("model error: stopped (invalid_request_error)"))
    }

    test("unknown events are skipped; a stream cut short may pass if sent again") {
      val text = event(
        """{"type":"content_block_start","index":0,"content_block":{"type":"text","text":"hi"}}"""
      ) ++ event("""{"type":"content_block_stop","index":0}""")
      val ended = event(
        """{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":2}}"""
      ) ++
        event("""{"type":"message_stop"}""")
      folded(started ++ event("""{"type":"future_event","x":1}""") ++ text ++ ended)._1
        .map(m => (m.blocks, m.stop)) ==> Right(
        (Vector(AssistantBlock.Text("hi")), StopReason.EndTurn)
      )
      folded(started ++ text)._1 ==> Left(
        ProviderError.Unavailable("unreadable stream: the stream ended before message_stop")
      )
    }

    test("a refusal partway through is Refused naming its category") {
      val refused = event(
        """{"type":"message_delta","delta":{"stop_reason":"refusal","stop_details":{"type":"refusal","category":"bio"}},"usage":{"output_tokens":3}}"""
      ) ++ event("""{"type":"message_stop"}""")
      folded(started ++ refused)._1 ==>
        Left(ProviderError.Refused("the model declined to answer (category: bio)"))
    }
  }
}
