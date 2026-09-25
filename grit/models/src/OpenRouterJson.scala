package grit.models

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.provider.{ModelRequest, ProviderError, Tool, ToolUse}

/** OpenRouter's chat-completions wire format (OpenAI's shape), both ways. Pure. Checked
  * against openrouter.ai/docs (API reference, reasoning tokens, errors) on 2026-09-23.
  */
object OpenRouterJson {

  /** The request body for `request` on `model`. A request with tools names them in
    * `tools`, with `tool_choice` `auto` or `none` (OpenRouter's tool-calling guide: every
    * request of a tool exchange sends the tools again); one without has neither key.
    */
  def request(model: String, maxTokens: Int, request: ModelRequest): ujson.Value = {
    val body = ujson.Obj(
      "model" -> model,
      "max_tokens" -> maxTokens,
      "messages" -> ujson.Arr.from(
        ujson.Obj("role" -> "system", "content" -> request.system) +: request.messages.map(
          message
        )
      )
    )
    if (request.tools.nonEmpty) {
      body("tools") = ujson.Arr.from(request.tools.map(tool))
      body("tool_choice") = request.use match {
        case ToolUse.Auto => "auto"
        case ToolUse.Off => "none"
      }
    }
    body
  }

  private def tool(t: Tool): ujson.Value =
    ujson.Obj(
      "type" -> "function",
      "function" -> ujson.Obj(
        "name" -> t.name,
        "description" -> t.description,
        "parameters" -> t.parameters
      )
    )

  private def message(m: Message): ujson.Value = m match {
    case Message.User(text) => ujson.Obj("role" -> "user", "content" -> text)
    case Message.ToolResult(callId, content, _) =>
      // The tool role has no error flag; an error is still a result the model reads.
      ujson.Obj(
        "role" -> "tool",
        "tool_call_id" -> ToolCallId.value(callId),
        "content" -> content
      )
    case Message.Assistant(blocks, _, _, _) =>
      val text = blocks.collect { case AssistantBlock.Text(t) => t }.mkString
      val calls = blocks.collect { case AssistantBlock.ToolCall(id, name, arguments) =>
        ujson.Obj(
          "id" -> ToolCallId.value(id),
          "type" -> "function",
          "function" -> ujson.Obj("name" -> name, "arguments" -> ujson.write(arguments))
        )
      }
      // Sent back verbatim, as OpenRouter asks, so the model's reasoning carries across
      // the calls of one turn.
      val replay = blocks.collectFirst { case AssistantBlock.Reasoning(_, Some(r)) => r }
      ujson.Obj.from(
        Vector[(String, ujson.Value)](
          "role" -> "assistant",
          "content" -> (if (text.isEmpty && calls.nonEmpty) ujson.Null else ujson.Str(text))
        ) ++ Option.when(calls.nonEmpty)("tool_calls" -> ujson.Arr.from(calls)) ++
          replay.map("reasoning_details" -> _)
      )
  }

  /** The assistant message in a 200 response, or why there is none. */
  def response(body: ujson.Value): Either[ProviderError, Message.Assistant] =
    for {
      root <- body.objOpt.toRight(unreadable("not an object"))
      choice <- root
        .get("choices")
        .flatMap(_.arrOpt)
        .flatMap(_.headOption)
        .flatMap(_.objOpt)
        .toRight(unreadable("no choices"))
      _ <- choice.get("error").flatMap(_.objOpt) match {
        case Some(error) => Left(ProviderError.Unavailable(s"model error: ${describe(error)}"))
        case None => Right(())
      }
      message <- choice.get("message").flatMap(_.objOpt).toRight(unreadable("no message"))
      blocks <- blockList(message)
    } yield Message.Assistant(
      blocks,
      stop(choice.get("finish_reason").flatMap(_.strOpt)),
      usage(root.get("usage").flatMap(_.objOpt)),
      root.get("model").flatMap(_.strOpt).getOrElse("unknown")
    )

  /** The error in a non-200 response with status `status`. */
  def error(status: Int, body: String): ProviderError = {
    val detail = scala.util
      .Try(ujson.read(body))
      .toOption
      .flatMap(_.objOpt)
      .flatMap(_.get("error"))
      .flatMap(_.objOpt)
      .map(describe)
      .getOrElse(body.take(200))
    ProviderError.Unavailable(s"HTTP $status: $detail")
  }

  private def describe(error: collection.Map[String, ujson.Value]): String = {
    val message = error.get("message").flatMap(_.strOpt).getOrElse("no message")
    val kind = error
      .get("metadata")
      .flatMap(_.objOpt)
      .flatMap(_.get("error_type"))
      .flatMap(_.strOpt)
    kind.fold(message)(k => s"$message ($k)")
  }

  private def blockList(
      message: collection.Map[String, ujson.Value]
  ): Either[ProviderError, Vector[AssistantBlock]] = {
    val reasoningText = message.get("reasoning").flatMap(_.strOpt).getOrElse("")
    val replay = message.get("reasoning_details").filter(_.arrOpt.exists(_.nonEmpty))
    val reasoning = Option.when(reasoningText.nonEmpty || replay.isDefined)(
      AssistantBlock.Reasoning(reasoningText, replay)
    )
    val text =
      message.get("content").flatMap(_.strOpt).filter(_.nonEmpty).map(AssistantBlock.Text(_))
    val calls =
      message.get("tool_calls").flatMap(_.arrOpt).getOrElse(Seq.empty).toVector.map { call =>
        for {
          c <- call.objOpt.toRight(unreadable("tool call is not an object"))
          id <- c.get("id").flatMap(_.strOpt).toRight(unreadable("tool call without an id"))
          f <- c
            .get("function")
            .flatMap(_.objOpt)
            .toRight(unreadable("tool call without a function"))
          name <- f.get("name").flatMap(_.strOpt).toRight(unreadable("tool call without a name"))
          raw = f.get("arguments").flatMap(_.strOpt).getOrElse("{}")
        } yield AssistantBlock.ToolCall(
          ToolCallId(id),
          name,
          // A model can send arguments that are not JSON; keep them for the tool to reject.
          scala.util.Try(ujson.read(raw)).getOrElse(ujson.Str(raw))
        )
      }
    calls
      .foldLeft[Either[ProviderError, Vector[AssistantBlock]]](Right(Vector.empty)) { (acc, c) =>
        acc.flatMap(done => c.map(done :+ _))
      }
      .map(toolCalls => reasoning.toVector ++ text.toVector ++ toolCalls)
  }

  private def stop(reason: Option[String]): StopReason = reason match {
    case Some("stop") => StopReason.EndTurn
    case Some("tool_calls") => StopReason.ToolUse
    case Some("length") => StopReason.MaxTokens
    case Some("content_filter") => StopReason.ContentFilter
    case Some(other) => StopReason.Other(other)
    case None => StopReason.Other("none")
  }

  private def usage(u: Option[collection.Map[String, ujson.Value]]): Usage = {
    def count(v: Option[ujson.Value]): Tokens = Tokens(v.flatMap(_.numOpt).fold(0L)(_.toLong))
    Usage(
      count(u.flatMap(_.get("prompt_tokens"))),
      count(u.flatMap(_.get("completion_tokens"))),
      count(
        u.flatMap(_.get("prompt_tokens_details")).flatMap(_.objOpt).flatMap(_.get("cached_tokens"))
      ),
      // ujson reads numbers as doubles; a double's shortest form is the decimal the
      // provider wrote for any cost it reports (at most ~15 significant digits).
      u.flatMap(_.get("cost")).flatMap(_.numOpt).map(d => BigDecimal(d.toString))
    )
  }

  private def unreadable(why: String): ProviderError =
    ProviderError.Unavailable(s"unreadable response: $why")
}
