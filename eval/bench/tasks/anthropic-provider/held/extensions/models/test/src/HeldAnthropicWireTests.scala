package grit.models

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.ReasoningReplay
import grit.core.provider.{ModelRequest, ProviderError}

import utest.*

/** Anthropic's wire form through `AnthropicJson.request` and `AnthropicJson.response`: the
  * request body read back as parsed JSON, and a response body read into an assistant message.
  */
object HeldAnthropicWireTests extends TestSuite {

  private val usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, None)

  private def called(blocks: AssistantBlock*): Message =
    Message.Assistant(blocks.toVector, StopReason.ToolUse, usage, "claude-haiku-5-5")

  /** The body `AnthropicJson.request` writes for `request` on `claude-haiku-5-5`, its effort
    * unset, earlier thinking sent back, and any other switch off. The brief's signature is
    * `request(model, maxTokens, effort, replay, request)`; the known change adds a `Boolean`
    * (`dropBlock`) before `request`. Each parameter is filled by its type, so a design with
    * either signature, in any order, is called the same way.
    */
  private def sent(request: ModelRequest): ujson.Value = {
    def argument(p: Class[?]): Option[AnyRef] =
      if (p == classOf[String]) Some("claude-haiku-5-5")
      else if (p == java.lang.Integer.TYPE) Some(Int.box(1024))
      else if (p == classOf[Option[?]]) Some(None)
      else if (p == classOf[ReasoningReplay]) Some(ReasoningReplay.Details)
      else if (p == java.lang.Boolean.TYPE) Some(java.lang.Boolean.FALSE)
      else if (p == classOf[ModelRequest]) Some(request)
      else None
    val writers = AnthropicJson.getClass.getMethods.toVector
      .filter(m => m.getName == "request" && m.getReturnType == classOf[ujson.Value])
      .flatMap { m =>
        val filled = m.getParameterTypes.toVector.map(argument)
        Option.when(filled.forall(_.isDefined))(m -> filled.flatten)
      }
    writers.headOption match {
      case Some((m, args)) =>
        // By arity, not `args*`: an array spliced from a vector is not pure under capture checking.
        (args match {
          case Vector(a, b, c, d, e) => m.invoke(AnthropicJson, a, b, c, d, e)
          case Vector(a, b, c, d, e, f) => m.invoke(AnthropicJson, a, b, c, d, e, f)
          case _ => throw new java.lang.AssertionError(s"AnthropicJson.request takes ${args.size}")
        }) match {
          case body: ujson.Value => body
          case other => throw new java.lang.AssertionError(s"AnthropicJson.request returned $other")
        }
      case None =>
        throw new java.lang.AssertionError(
          "no AnthropicJson.request over (model, maxTokens, effort, replay[, Boolean], request)"
        )
    }
  }

  /** Every content block of every message in `body`, in order. */
  private def blocks(body: ujson.Value): Vector[ujson.Value] =
    body("messages").arr.toVector.flatMap(_("content").arr)

  /** A 200 response body as Anthropic documents it, with `content`, `stop_reason` and
    * `stop_details` as given.
    */
  private def replied(
      content: ujson.Arr,
      stop: String,
      details: ujson.Value = ujson.Null
  ): ujson.Value =
    ujson.Obj(
      "id" -> "msg_01",
      "type" -> "message",
      "role" -> "assistant",
      "model" -> "claude-haiku-5-5",
      "content" -> content,
      "stop_reason" -> stop,
      "stop_sequence" -> ujson.Null,
      "stop_details" -> details,
      "usage" -> ujson.Obj(
        "input_tokens" -> 2000,
        "cache_creation_input_tokens" -> 3000,
        "cache_read_input_tokens" -> 6000,
        "output_tokens" -> 1000
      )
    )

  private val checking = ujson.Obj("type" -> "text", "text" -> "Checking.")

  val tests = Tests {
    test("request: an empty system prompt sends no system block, so no breakpoint on one") {
      val body = sent(ModelRequest("", Vector(Message.User("hi"))))
      body.obj.get("system").flatMap(_.arrOpt).fold(0)(_.size) ==> 0
    }

    test(
      "request: the stable breakpoint never sits on a thinking block, and the recorded thinking is sent and kept byte for byte"
    ) {
      val recorded =
        ujson.Obj("type" -> "thinking", "thinking" -> "Read it first.", "signature" -> "EqQBsig")
      val before = ujson.write(recorded)
      val body = sent(
        ModelRequest(
          "sys",
          Vector(
            Message.User("hi"),
            called(
              AssistantBlock.Text("ok"),
              AssistantBlock.Reasoning("Read it first.", Some(recorded))
            )
          ),
          stable = 2
        )
      )
      blocks(body).filter(_.obj.contains("cache_control")).map(_("type").str) ==> Vector("text")
      blocks(body).filter(_("type").str == "thinking").map(ujson.write(_)) ==> Vector(before)
      ujson.write(recorded) ==> before
    }

    test("request: a tool call's input is a JSON object, {} when its arguments are not one") {
      def input(arguments: ujson.Value): Vector[ujson.Value] =
        blocks(
          sent(
            ModelRequest(
              "sys",
              Vector(
                Message.User("hi"),
                called(AssistantBlock.ToolCall(ToolCallId("toolu_1"), "read", arguments)),
                Message.ToolResult(ToolCallId("toolu_1"), "contents", isError = false)
              )
            )
          )
        ).filter(_("type").str == "tool_use").map(_("input"))
      input(ujson.Str("{path")) ==> Vector(ujson.Obj())
      input(ujson.Obj("path" -> "a")) ==> Vector(ujson.Obj("path" -> "a"))
    }

    test("response: a content block of a type grit does not know is skipped, the rest read") {
      val body = replied(
        ujson.Arr(
          checking,
          ujson.Obj("type" -> "a_block_added_later", "id" -> "x_1"),
          ujson.Obj(
            "type" -> "tool_use",
            "id" -> "toolu_1",
            "name" -> "read",
            "input" -> ujson.Obj()
          )
        ),
        "tool_use"
      )
      AnthropicJson.response(body).map(_.blocks) ==> Right(
        Vector(
          AssistantBlock.Text("Checking."),
          AssistantBlock.ToolCall(ToolCallId("toolu_1"), "read", ujson.Obj())
        )
      )
    }

    test("response: each stop reason read, the ones grit has no case for kept by their word") {
      def stop(reason: String) =
        AnthropicJson.response(replied(ujson.Arr(checking), reason)).map(_.stop)
      stop("end_turn") ==> Right(StopReason.EndTurn)
      stop("tool_use") ==> Right(StopReason.ToolUse)
      stop("max_tokens") ==> Right(StopReason.MaxTokens)
      stop("stop_sequence") ==> Right(StopReason.Other("stop_sequence"))
      stop("pause_turn") ==> Right(StopReason.Other("pause_turn"))
      stop("model_context_window_exceeded") ==>
        Right(StopReason.Other("model_context_window_exceeded"))
    }

    test("response: a refusal stop reason is Refused, its cause naming the category") {
      val refused = AnthropicJson.response(
        replied(
          ujson.Arr(checking),
          "refusal",
          ujson.Obj("type" -> "refusal", "category" -> "cyber", "explanation" -> ujson.Null)
        )
      )
      refused.left.map {
        case ProviderError.Refused(cause) => cause.contains("cyber")
        case _ => false
      } ==> Left(true)
    }

    test("response: input counts the uncached, cache-read and cache-written tokens together") {
      AnthropicJson.response(replied(ujson.Arr(checking), "end_turn")).map { m =>
        (m.usage.input, m.usage.output, m.usage.cacheRead, m.usage.cacheWrite)
      } ==> Right((Tokens(11000), Tokens(1000), Tokens(6000), Tokens(3000)))
    }
  }
}
