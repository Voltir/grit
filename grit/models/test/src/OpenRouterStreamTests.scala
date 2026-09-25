package grit.models

import grit.core.message.{AssistantBlock, StopReason, Tokens}
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

    test("an error after the 200 is the call's error") {
      val (reply, _) = folded(
        Vector(
          chunk("""{"content":"Fe"}"""),
          """data: {"error":{"message":"upstream gave up"},"choices":[{"delta":{},"finish_reason":"error"}]}"""
        )
      )
      reply ==> Left(ProviderError.Unavailable("model error: upstream gave up"))
    }

    test("a stream cut short, or not JSON, is unreadable, not a message") {
      val (cut, _) = folded(Vector(chunk("""{"content":"Fe"}""")))
      cut ==> Left(ProviderError.Unavailable("unreadable stream: the stream ended before [DONE]"))
      val (garbled, _) = folded(Vector("data: {nope"))
      assert(garbled.isLeft)
    }

    test("the stub streams its reply in pieces that join back to it") {
      val told = Vector.newBuilder[Delta]
      val reply = new StubProvider()
        .stream(ModelRequest("s", Vector(grit.core.message.Message.User("a b c"))), d => told += d)
      val text = reply.map(_.blocks.collect { case AssistantBlock.Text(t) => t }.mkString)
      val pieces = told.result().collect { case Delta.Text(t) => t }
      assert(pieces.size > 1)
      text ==> Right(pieces.mkString)
    }

    test("the stub calls the first tool it may, with the #call: arguments, beside a line of text") {
      val topic = grit.core.provider.Tool("topic", "d", ujson.Obj())
      val said = """hi #call:{"about":"new","name":"Knots"}"""
      val asked = grit.core.message.Message.User(said)
      val called = new StubProvider().complete(ModelRequest("s", Vector(asked), Vector(topic)))
      called.map(_.blocks) ==> Right(
        Vector(
          AssistantBlock.Text("stub calls topic"),
          AssistantBlock.ToolCall(
            StubProvider.CallId,
            "topic",
            ujson.Obj("about" -> "new", "name" -> "Knots")
          )
        )
      )
      // Not while it may not, nor after the tool's result: then it quotes the user.
      val off = new StubProvider()
        .complete(
          ModelRequest("s", Vector(asked), Vector(topic), grit.core.provider.ToolUse.Off)
        )
      off.map(_.blocks.size) ==> Right(1)
      val after = new StubProvider().complete(
        ModelRequest(
          "s",
          Vector(
            asked,
            called.getOrElse(sys.error("stub")),
            grit.core.message.Message.ToolResult(StubProvider.CallId, "noted", false)
          ),
          Vector(topic)
        )
      )
      after.map(_.blocks) ==> Right(Vector(AssistantBlock.Text(s"stub reply to: $said")))
      StubProvider.arguments("no marker") ==> ujson.Obj()
    }
  }
}
