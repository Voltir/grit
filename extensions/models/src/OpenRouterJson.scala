package grit.models

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Effort, ReasoningReplay, Upstream}
import grit.core.provider.{ModelRequest, ProviderError, ToolSchema, ToolUse}

/** OpenRouter's chat-completions wire format (OpenAI's shape), both ways. Pure. Checked
  * against openrouter.ai/docs (API reference, reasoning tokens, errors) on 2026-09-23, and its
  * parameters reference (`tool_choice` accepts `"required"`) on 2026-10-08.
  */
object OpenRouterJson {

  /** The request body for `request` on `model`, served by `upstream` alone with no fallback,
    * or by whichever upstream OpenRouter picks when `None`. A request with
    * tools names them in `tools`, with `tool_choice` `auto`, `none` or `required` (OpenRouter's
    * tool-calling guide: every request of a tool exchange sends the tools again); one
    * without has neither key. Each tool is sent `strict` as its [[ToolSchema]] says. `effort`,
    * when set, asks the model to reason that hard; an assistant message's reasoning goes back
    * as `replay` says, when it is in OpenRouter's form (`reasoning_details`), and never
    * otherwise.
    *
    * For a model under [[AnthropicProvider.Vendor]], every text that is not empty goes as
    * text parts, an empty system prompt is left out, and its breakpoints are those
    * [[CacheMarks.marked]] places. Any other model's body has none, and its texts are strings.
    */
  def request(
      model: String,
      maxTokens: Int,
      upstream: Option[Upstream],
      request: ModelRequest,
      effort: Option[Effort] = None,
      replay: ReasoningReplay = ReasoningReplay.Details
  ): ujson.Value = {
    val cached = model.startsWith(AnthropicProvider.Vendor)
    val tools = request.tools.map(tool)
    val messages = request.messages.map(message(_, replay, cached))
    val system =
      if (!cached) Some(ujson.Obj("role" -> "system", "content" -> request.system))
      else
        CacheMarks
          .marked(request.system, tools, messages.map(Some(_)), request.stable)
          .map(block => ujson.Obj("role" -> "system", "content" -> ujson.Arr(block)))
    val body = ujson.Obj(
      "model" -> model,
      "max_tokens" -> maxTokens,
      "messages" -> ujson.Arr.from(system.toVector ++ messages)
    )
    effort.foreach(e => body("reasoning") = ujson.Obj("effort" -> e.word))
    upstream.foreach { u =>
      body("provider") =
        ujson.Obj("order" -> ujson.Arr(Upstream.value(u)), "allow_fallbacks" -> false)
    }
    if (tools.nonEmpty) {
      body("tools") = ujson.Arr.from(tools)
      body("tool_choice") = request.use match {
        case ToolUse.Auto => "auto"
        case ToolUse.Off => "none"
        case ToolUse.Required => "required"
      }
    }
    body
  }

  /** A tool; `strict` is sent only when set, so a provider that does not know it never sees it. */
  private def tool(t: ToolSchema): ujson.Obj = {
    val function = ujson.Obj(
      "name" -> t.name,
      "description" -> t.description,
      "parameters" -> t.parameters
    )
    if (t.strict) function("strict") = true
    ujson.Obj("type" -> "function", "function" -> function)
  }

  /** `m` on the wire, each text as text parts when `parts` and it is not empty (an empty part
    * is refused), as a string otherwise.
    */
  private def message(m: Message, replay: ReasoningReplay, parts: Boolean): ujson.Obj = {
    def said(text: String): ujson.Value =
      if (parts && text.nonEmpty) ujson.Arr(ujson.Obj("type" -> "text", "text" -> text))
      else ujson.Str(text)
    m match {
      case Message.User(text) => ujson.Obj("role" -> "user", "content" -> said(text))
      case Message.ToolResult(callId, content, _) =>
        // The tool role has no error flag; an error is still a result the model reads.
        ujson.Obj(
          "role" -> "tool",
          "tool_call_id" -> ToolCallId.value(callId),
          "content" -> said(content)
        )
      case Message.Assistant(blocks, _, _, _, _) =>
        val text = blocks.collect { case AssistantBlock.Text(t) => t }.mkString
        val calls = blocks.collect { case AssistantBlock.ToolCall(id, name, arguments) =>
          ujson.Obj(
            "id" -> ToolCallId.value(id),
            "type" -> "function",
            "function" -> ujson.Obj("name" -> name, "arguments" -> ujson.write(arguments))
          )
        }
        // Sent back verbatim, as OpenRouter asks, so the model's reasoning carries across
        // the calls of one turn; or not at all, for a pair whose replay is dropped. Only
        // OpenRouter's own form, an array: another provider's would be garbage here.
        val details = replay match {
          case ReasoningReplay.Details =>
            blocks.collectFirst { case AssistantBlock.Reasoning(_, Some(r: ujson.Arr)) => r }
          case ReasoningReplay.Dropped => None
        }
        ujson.Obj.from(
          Vector[(String, ujson.Value)](
            "role" -> "assistant",
            "content" -> (if (text.isEmpty && calls.nonEmpty) ujson.Null else said(text))
          ) ++ Option.when(calls.nonEmpty)("tool_calls" -> ujson.Arr.from(calls)) ++
            details.map("reasoning_details" -> _)
        )
    }
  }

  /** The assistant message in a 200 response, or why there is none: a top-level or a
    * choice's `error` (an upstream can refuse after the 200) is `model error: …`. Its
    * `upstream` is the response's `provider`, the name of the upstream that served it.
    */
  def response(body: ujson.Value): Either[ProviderError, Message.Assistant] =
    for {
      root <- body.objOpt.toRight(unreadable("not an object"))
      _ <- errorIn(root)
      choice <- root
        .get("choices")
        .flatMap(_.arrOpt)
        .flatMap(_.headOption)
        .flatMap(_.objOpt)
        .toRight(unreadable("no choices"))
      _ <- errorIn(choice)
      message <- choice.get("message").flatMap(_.objOpt).toRight(unreadable("no message"))
      blocks <- blockList(message)
    } yield Message.Assistant(
      blocks,
      stop(choice.get("finish_reason").flatMap(_.strOpt)),
      usage(root.get("usage").flatMap(_.objOpt)),
      root.get("model").flatMap(_.strOpt).getOrElse("unknown"),
      root.get("provider").flatMap(_.strOpt)
    )

  /** The error in a non-200 response with status `status`: [[ProviderError.Unavailable]]
    * for a status another try may get past ([[ProviderHttp.transient]]),
    * [[ProviderError.Refused]] for any other.
    */
  def error(status: Int, body: String): ProviderError = {
    val detail = scala.util
      .Try(ujson.read(body))
      .toOption
      .flatMap(_.objOpt)
      .flatMap(_.get("error"))
      .flatMap(_.objOpt)
      .map(describe)
      .getOrElse(body.trim.take(200))
    val cause = s"HTTP $status: $detail"
    if (ProviderHttp.transient(status)) ProviderError.Unavailable(cause)
    else ProviderError.Refused(cause)
  }

  /** The model error `error`, an OpenRouter error object sent after the 200, as `model
    * error: …`: [[ProviderError.Unavailable]] when its `code` is a [[ProviderHttp.transient]]
    * status, [[ProviderError.Refused]] otherwise.
    */
  def modelError(error: collection.Map[String, ujson.Value]): ProviderError = {
    val cause = s"model error: ${describe(error)}"
    error.get("code").flatMap(_.numOpt).filter(_.isWhole).map(_.toInt) match {
      case Some(code) if ProviderHttp.transient(code) => ProviderError.Unavailable(cause)
      case _ => ProviderError.Refused(cause)
    }
  }

  private def errorIn(at: collection.Map[String, ujson.Value]): Either[ProviderError, Unit] =
    at.get("error").flatMap(_.objOpt) match {
      case Some(error) => Left(modelError(error))
      case None => Right(())
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
        } yield AssistantBlock.ToolCall(
          ToolCallId(id),
          name,
          ToolArguments.read(f.get("arguments"))
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
    val details = u.flatMap(_.get("prompt_tokens_details")).flatMap(_.objOpt)
    Usage(
      count(u.flatMap(_.get("prompt_tokens"))),
      count(u.flatMap(_.get("completion_tokens"))),
      count(details.flatMap(_.get("cached_tokens"))),
      // ujson reads numbers as doubles; a double's shortest form is the decimal the
      // provider wrote for any cost it reports (at most ~15 significant digits).
      u.flatMap(_.get("cost")).flatMap(_.numOpt).map(d => BigDecimal(d.toString)),
      count(details.flatMap(_.get("cache_write_tokens")))
    )
  }

  private def unreadable(why: String): ProviderError =
    ProviderError.Refused(s"unreadable response: $why")
}
