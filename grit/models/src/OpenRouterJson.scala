package grit.models

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Effort, ReasoningReplay, Upstream}
import grit.core.provider.{ModelRequest, ProviderError, ToolSchema, ToolUse}

/** OpenRouter's chat-completions wire format (OpenAI's shape), both ways. Pure. Checked
  * against openrouter.ai/docs (API reference, reasoning tokens, errors) on 2026-09-23.
  */
object OpenRouterJson {

  /** The request body for `request` on `model`, served by `upstream` alone with no fallback,
    * or by whichever upstream OpenRouter picks when `None`. A request with
    * tools names them in `tools`, with `tool_choice` `auto` or `none` (OpenRouter's
    * tool-calling guide: every request of a tool exchange sends the tools again); one
    * without has neither key. Each tool is sent `strict` as its [[ToolSchema]] says. `effort`,
    * when set, asks the model to reason that hard; an assistant message's reasoning goes back
    * as `replay` says.
    */
  def request(
      model: String,
      maxTokens: Int,
      upstream: Option[Upstream],
      request: ModelRequest,
      effort: Option[Effort] = None,
      replay: ReasoningReplay = ReasoningReplay.Details
  ): ujson.Value = {
    val body = ujson.Obj(
      "model" -> model,
      "max_tokens" -> maxTokens,
      "messages" -> ujson.Arr.from(
        ujson.Obj("role" -> "system", "content" -> request.system) +: request.messages.map(
          message(_, replay)
        )
      )
    )
    effort.foreach(e => body("reasoning") = ujson.Obj("effort" -> effortWord(e)))
    upstream.foreach { u =>
      body("provider") =
        ujson.Obj("order" -> ujson.Arr(Upstream.value(u)), "allow_fallbacks" -> false)
    }
    if (request.tools.nonEmpty) {
      body("tools") = ujson.Arr.from(request.tools.map(tool))
      body("tool_choice") = request.use match {
        case ToolUse.Auto => "auto"
        case ToolUse.Off => "none"
      }
    }
    body
  }

  /** A tool; `strict` is sent only when set, so a provider that does not know it never sees it. */
  private def tool(t: ToolSchema): ujson.Value = {
    val function = ujson.Obj(
      "name" -> t.name,
      "description" -> t.description,
      "parameters" -> t.parameters
    )
    if (t.strict) function("strict") = true
    ujson.Obj("type" -> "function", "function" -> function)
  }

  /** OpenRouter's word for `e`. */
  private def effortWord(e: Effort): String = e match {
    case Effort.Minimal => "minimal"
    case Effort.Low => "low"
    case Effort.Medium => "medium"
    case Effort.High => "high"
    case Effort.XHigh => "xhigh"
    case Effort.Max => "max"
  }

  private def message(m: Message, replay: ReasoningReplay): ujson.Value = m match {
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
      // the calls of one turn; or not at all, for a pair whose replay is dropped.
      val details = replay match {
        case ReasoningReplay.Details =>
          blocks.collectFirst { case AssistantBlock.Reasoning(_, Some(r)) => r }
        case ReasoningReplay.Dropped => None
      }
      ujson.Obj.from(
        Vector[(String, ujson.Value)](
          "role" -> "assistant",
          "content" -> (if (text.isEmpty && calls.nonEmpty) ujson.Null else ujson.Str(text))
        ) ++ Option.when(calls.nonEmpty)("tool_calls" -> ujson.Arr.from(calls)) ++
          details.map("reasoning_details" -> _)
      )
  }

  /** The assistant message in a 200 response, or why there is none: a top-level or a
    * choice's `error` (an upstream can refuse after the 200) is `model error: …`.
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
      root.get("model").flatMap(_.strOpt).getOrElse("unknown")
    )

  /** The error in a non-200 response with status `status`: [[ProviderError.Unavailable]]
    * for a status another try may get past ([[transient]]), [[ProviderError.Refused]] for any
    * other.
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
    if (transient(status)) ProviderError.Unavailable(cause) else ProviderError.Refused(cause)
  }

  /** Whether a request answered with HTTP `status` may succeed if sent again: 408, 429 and
    * every 5xx.
    */
  def transient(status: Int): Boolean = status == 408 || status == 429 || status / 100 == 5

  /** The model error `error`, an OpenRouter error object sent after the 200, as `model
    * error: …`: [[ProviderError.Unavailable]] when its `code` is a [[transient]] status,
    * [[ProviderError.Refused]] otherwise.
    */
  def modelError(error: collection.Map[String, ujson.Value]): ProviderError = {
    val cause = s"model error: ${describe(error)}"
    error.get("code").flatMap(_.numOpt).filter(_.isWhole).map(_.toInt) match {
      case Some(code) if transient(code) => ProviderError.Unavailable(cause)
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
        } yield AssistantBlock.ToolCall(ToolCallId(id), name, arguments(f.get("arguments")))
      }
    calls
      .foldLeft[Either[ProviderError, Vector[AssistantBlock]]](Right(Vector.empty)) { (acc, c) =>
        acc.flatMap(done => c.map(done :+ _))
      }
      .map(toolCalls => reasoning.toVector ++ text.toVector ++ toolCalls)
  }

  /** A call's `arguments` as `AssistantBlock.ToolCall` holds them. The wire sends a JSON
    * string; an upstream that sends the object itself is taken at its word.
    */
  private def arguments(sent: Option[ujson.Value]): ujson.Value = sent match {
    case Some(ujson.Str(raw)) if raw.trim.nonEmpty =>
      // Not JSON: kept whole, for the loop's reader to refuse and echo.
      scala.util.Try(ujson.read(raw)).getOrElse(ujson.Str(raw))
    case Some(o: ujson.Obj) => o
    case _ => ujson.Obj()
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
    ProviderError.Refused(s"unreadable response: $why")
}
