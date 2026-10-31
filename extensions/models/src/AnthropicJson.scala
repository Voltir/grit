package grit.models

import grit.core.id.ToolCallId
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.model.{Effort, ReasoningReplay, Upstream}
import grit.core.provider.{ModelRequest, ProviderError, ToolSchema, ToolUse}

/** Anthropic's Messages API wire format, both ways. Pure. Checked against platform.claude.com
  * ("Using the Messages API", "Prompt caching", "Streaming messages", the Messages API
  * reference) on 2026-10-29, at API version [[Version]].
  */
object AnthropicJson {

  /** The `anthropic-version` every request is sent under. */
  val Version = "2026-10-21"

  /** The `anthropic-beta` a request must be sent under when it asks [[request]]'s `dropBlock`. */
  val BindingBeta = "thinking-binding-controls-2026-08-01"

  /** Whether `model` (Anthropic's own id) is `alias` or a dated snapshot of it
    * (`claude-haiku-5-5-20261001` of `claude-haiku-5-5`).
    */
  def isOrSnapshotOf(model: String, alias: String): Boolean =
    model == alias || model.startsWith(alias + "-")

  /** The request body for `request` on `model` (Anthropic's own id), its thinking adaptive and
    * summarised and its effort `effort` when set. Each [[Message]] is one wire message, in
    * order: a user's empty text, and an assistant message left with nothing to send, are
    * left out, since the API refuses an empty block. A tool call's arguments go back as sent
    * when they are an object, else as `{}`. Thinking goes back verbatim in its place when
    * `replay` is [[ReasoningReplay.Details]] and it is in Anthropic's form, and never
    * otherwise. Tools are sent `strict` as each [[ToolSchema]] says, with `tool_choice` `auto`
    * for [[ToolUse.Auto]] and [[ToolUse.Required]] (Anthropic's current models refuse a forced
    * choice) and `none` for [[ToolUse.Off]]; a request without tools has neither key.
    *
    * Its cache breakpoints are those [[CacheMarks.marked]] places. `dropBlock` asks the API to drop a
    * thinking block an earlier edit to the request has invalidated, and every one after it,
    * rather than refuse the request; it needs [[BindingBeta]].
    */
  def request(
      model: String,
      maxTokens: Int,
      effort: Option[Effort],
      replay: ReasoningReplay,
      dropBlock: Boolean,
      request: ModelRequest
  ): ujson.Value = {
    val thinking = ujson.Obj("type" -> "adaptive", "display" -> "summarized")
    if (dropBlock)
      thinking("block_binding") = ujson.Obj("prefix_mismatch_behavior" -> "drop_block")
    val body = ujson.Obj("model" -> model, "max_tokens" -> maxTokens, "thinking" -> thinking)
    effort.foreach(e => body("output_config") = ujson.Obj("effort" -> effortWord(e)))
    val tools = request.tools.map(tool)
    if (tools.nonEmpty) {
      body("tools") = ujson.Arr.from(tools)
      body("tool_choice") = ujson.Obj("type" -> (request.use match {
        case ToolUse.Auto | ToolUse.Required => "auto"
        case ToolUse.Off => "none"
      }))
    }
    val messages = request.messages.map(message(_, replay))
    CacheMarks
      .marked(request.system, tools, messages, request.stable)
      .foreach(block => body("system") = ujson.Arr(block))
    body("messages") = ujson.Arr.from(messages.flatten)
    body
  }

  /** What Anthropic's cache keys the last breakpoint of `body` (a [[request]]) on, as text:
    * each tool, each block of the system prompt, and each message's role and blocks through
    * the one that breakpoint marks, in that order, one to a line, each written as `body`
    * holds it but without `cache_control`. A later request can read what this one wrote to
    * the cache only when its own prefix begins with this one. Empty when `body` marks no
    * breakpoint.
    */
  def prefix(body: ujson.Value): String = {
    def items(key: String): Vector[ujson.Value] =
      body.obj.get(key).flatMap(_.arrOpt).fold(Vector.empty)(_.toVector)
    val pieces = items("tools") ++ items("system") ++ items("messages").flatMap { m =>
      ujson.Obj("role" -> m.obj.get("role").getOrElse(ujson.Null)) +:
        m.obj.get("content").flatMap(_.arrOpt).fold(Vector.empty)(_.toVector)
    }
    val marked = pieces.lastIndexWhere(_.objOpt.exists(_.contains("cache_control")))
    pieces
      .take(marked + 1)
      .map { piece =>
        // Copied key by key: a filtered ujson.Obj loses its key order.
        val unmarked = piece.objOpt.fold(piece) { o =>
          val copy = ujson.Obj()
          o.foreach { case (k, v) => if (k != "cache_control") copy(k) = v }
          copy
        }
        ujson.write(unmarked) + "\n"
      }
      .mkString
  }

  /** The assistant message in a 200 response, or why there is none: a `refusal` stop reason is
    * [[ProviderError.Refused]] naming its category. Its blocks keep the response's order, each
    * thinking block kept whole as its `replay`; `input` counts uncached, read and written
    * tokens; its cost is [[AnthropicRates.cost]]'s; its upstream is [[Upstream.AnthropicApi]].
    */
  def response(body: ujson.Value): Either[ProviderError, Message.Assistant] =
    for {
      root <- body.objOpt.toRight(ProviderHttp.unreadable("not an object"))
      _ <- refusal(root)
      content <- root
        .get("content")
        .flatMap(_.arrOpt)
        .toRight(ProviderHttp.unreadable("no content"))
      blocks <- content.toVector
        .map(blockOf)
        .foldLeft[Either[ProviderError, Vector[AssistantBlock]]](Right(Vector.empty)) { (acc, b) =>
          acc.flatMap(done => b.map(done ++ _))
        }
    } yield {
      val model = root.get("model").flatMap(_.strOpt).getOrElse("unknown")
      val counted = usage(root.get("usage").flatMap(_.objOpt))
      Message.Assistant(
        blocks,
        stop(root.get("stop_reason").flatMap(_.strOpt)),
        counted.copy(costUsd = AnthropicRates.cost(model, counted)),
        model,
        Some(Upstream.value(Upstream.AnthropicApi))
      )
    }

  /** [[ProviderHttp.httpError]], its detail `<message> (<type>)`. */
  def error(status: Int, body: String): ProviderError =
    ProviderHttp.httpError(status, body, describe)

  /** An `error` event sent after the 200, as `model error: <message> (<type>)`:
    * [[ProviderError.Unavailable]] when its type is `overloaded_error` or `api_error`,
    * [[ProviderError.Refused]] for any other.
    */
  def modelError(error: collection.Map[String, ujson.Value]): ProviderError = {
    val cause = s"model error: ${describe(error)}"
    error.get("type").flatMap(_.strOpt) match {
      case Some("overloaded_error" | "api_error") => ProviderError.Unavailable(cause)
      case _ => ProviderError.Refused(cause)
    }
  }

  private def tool(t: ToolSchema): ujson.Obj = {
    val wire =
      ujson.Obj("name" -> t.name, "description" -> t.description, "input_schema" -> t.parameters)
    if (t.strict) wire("strict") = true
    wire
  }

  /** Anthropic's word for `e`: [[Effort.word]], but `low` for [[Effort.Minimal]], since it has
    * none below `low`.
    */
  private def effortWord(e: Effort): String = e match {
    case Effort.Minimal => Effort.Low.word
    case other => other.word
  }

  private def message(m: Message, replay: ReasoningReplay): Option[ujson.Obj] = m match {
    case Message.User(text) =>
      Option.when(text.nonEmpty)(wire("user", ujson.Obj("type" -> "text", "text" -> text)))
    case Message.ToolResult(callId, content, isError) =>
      val result = ujson.Obj(
        "type" -> "tool_result",
        "tool_use_id" -> ToolCallId.value(callId),
        "content" -> content
      )
      if (isError) result("is_error") = true
      Some(wire("user", result))
    case Message.Assistant(blocks, _, _, _, _) =>
      val sent = blocks.flatMap {
        case AssistantBlock.Text(text) =>
          Option.when(text.nonEmpty)(ujson.Obj("type" -> "text", "text" -> text))
        case AssistantBlock.ToolCall(id, name, arguments) =>
          Some(
            ujson.Obj(
              "type" -> "tool_use",
              "id" -> ToolCallId.value(id),
              "name" -> name,
              "input" -> (arguments match {
                case o: ujson.Obj => o
                case _ => ujson.Obj()
              })
            )
          )
        case AssistantBlock.Reasoning(_, Some(r: ujson.Obj))
            if replay == ReasoningReplay.Details &&
              r.value.get("type").flatMap(_.strOpt).exists(CacheMarks.Thinking.contains) =>
          // A copy, so the breakpoint search can never mark the recorded block itself.
          Some(ujson.Obj.from(r.value))
        case AssistantBlock.Reasoning(_, _) => None
      }
      Option.when(sent.nonEmpty)(wire("assistant", sent*))
  }

  private def wire(role: String, content: ujson.Obj*): ujson.Obj =
    ujson.Obj("role" -> role, "content" -> ujson.Arr.from(content))

  private def refusal(root: collection.Map[String, ujson.Value]): Either[ProviderError, Unit] =
    root.get("stop_reason").flatMap(_.strOpt) match {
      case Some("refusal") =>
        // Branch on stop_reason alone: stop_details may be null even on a refusal.
        val category = root
          .get("stop_details")
          .flatMap(_.objOpt)
          .flatMap(_.get("category"))
          .flatMap(_.strOpt)
        Left(
          ProviderError.Refused(
            s"the model declined to answer (${category.fold("no category")(c => s"category: $c")})"
          )
        )
      case _ => Right(())
    }

  /** A content block as grit holds it: none for a block type grit does not know. */
  private def blockOf(block: ujson.Value): Either[ProviderError, Vector[AssistantBlock]] = {
    val b = block.objOpt.getOrElse(collection.mutable.LinkedHashMap.empty[String, ujson.Value])
    def str(key: String) = b.get(key).flatMap(_.strOpt)
    b.get("type").flatMap(_.strOpt) match {
      case Some("text") =>
        Right(str("text").filter(_.nonEmpty).map(AssistantBlock.Text(_)).toVector)
      case Some("thinking") =>
        Right(Vector(AssistantBlock.Reasoning(str("thinking").getOrElse(""), Some(block))))
      case Some("redacted_thinking") => Right(Vector(AssistantBlock.Reasoning("", Some(block))))
      case Some("tool_use") =>
        for {
          id <- str("id").toRight(ProviderHttp.unreadable("tool call without an id"))
          name <- str("name").toRight(ProviderHttp.unreadable("tool call without a name"))
        } yield Vector(
          AssistantBlock.ToolCall(ToolCallId(id), name, ToolArguments.read(b.get("input")))
        )
      case _ => Right(Vector.empty)
    }
  }

  private def stop(reason: Option[String]): StopReason = reason match {
    case Some("end_turn") => StopReason.EndTurn
    case Some("tool_use") => StopReason.ToolUse
    case Some("max_tokens") => StopReason.MaxTokens
    case Some(other) => StopReason.Other(other)
    case None => StopReason.Other("none")
  }

  /** `input_tokens` counts only what was neither read from nor written to the cache. */
  private def usage(u: Option[collection.Map[String, ujson.Value]]): Usage = {
    def count(key: String): Tokens =
      Tokens(u.flatMap(_.get(key)).flatMap(_.numOpt).fold(0L)(_.toLong))
    val read = count("cache_read_input_tokens")
    val write = count("cache_creation_input_tokens")
    Usage(count("input_tokens") + read + write, count("output_tokens"), read, None, write)
  }

  private def describe(error: collection.Map[String, ujson.Value]): String = {
    val message = error.get("message").flatMap(_.strOpt).getOrElse("no message")
    error.get("type").flatMap(_.strOpt).fold(message)(t => s"$message ($t)")
  }

}
