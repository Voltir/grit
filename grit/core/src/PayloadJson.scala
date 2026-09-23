package grit.core

/** The stored JSON form of a [[Payload]]. Written by hand, not derived: it is
  * persisted data, so a rename in Scala must not change it, and reading it is
  * total. (upickle's derivation also cannot see through capture checking's
  * inferred-type annotations.)
  */
object PayloadJson {

  def write(p: Payload): ujson.Value = p match {
    case Payload.Message(m) => ujson.Obj("kind" -> "message", "message" -> message(m))
  }

  /** The payload `v` encodes, or why it encodes none. */
  def read(v: ujson.Value): Either[String, Payload] =
    for {
      o <- obj(v)
      kind <- str(o, "kind")
      p <- kind match {
        case "message" => field(o, "message").flatMap(readMessage).map(Payload.Message(_))
        case other => Left(s"unknown payload kind: $other")
      }
    } yield p

  private def message(m: Message): ujson.Value = m match {
    case Message.User(text) => ujson.Obj("role" -> "user", "text" -> text)
    case Message.Assistant(blocks, stop, usage, model) =>
      val base = ujson.Obj(
        "role" -> "assistant",
        "blocks" -> ujson.Arr.from(blocks.map(block)),
        "usage" -> usageJson(usage),
        "model" -> model
      )
      stop match {
        case StopReason.EndTurn => base("stop") = "end_turn"
        case StopReason.ToolUse => base("stop") = "tool_use"
        case StopReason.MaxTokens => base("stop") = "max_tokens"
        case StopReason.ContentFilter => base("stop") = "content_filter"
        case StopReason.Other(raw) => base("stop") = "other"; base("stopRaw") = raw
      }
      base
    case Message.ToolResult(callId, content, isError) =>
      ujson.Obj(
        "role" -> "tool_result",
        "callId" -> ToolCallId.value(callId),
        "content" -> content,
        "isError" -> isError
      )
  }

  private def block(b: AssistantBlock): ujson.Value = b match {
    case AssistantBlock.Text(text) => ujson.Obj("type" -> "text", "text" -> text)
    case AssistantBlock.Reasoning(text, replay) =>
      val o = ujson.Obj("type" -> "reasoning", "text" -> text)
      replay.foreach(r => o("replay") = r)
      o
    case AssistantBlock.ToolCall(id, name, arguments) =>
      ujson.Obj(
        "type" -> "tool_call",
        "id" -> ToolCallId.value(id),
        "name" -> name,
        "arguments" -> arguments
      )
  }

  // Cost is a string so the provider's decimal survives exactly.
  private def usageJson(u: Usage): ujson.Value = {
    val o = ujson.Obj(
      "input" -> Tokens.value(u.input).toDouble,
      "output" -> Tokens.value(u.output).toDouble,
      "cachedInput" -> Tokens.value(u.cachedInput).toDouble
    )
    u.costUsd.foreach(c => o("costUsd") = c.toString)
    o
  }

  private def readMessage(v: ujson.Value): Either[String, Message] =
    for {
      o <- obj(v)
      role <- str(o, "role")
      m <- role match {
        case "user" => str(o, "text").map(Message.User(_))
        case "assistant" =>
          for {
            blocks <- arr(o, "blocks").flatMap(traverse(_)(readBlock))
            stop <- readStop(o)
            usage <- field(o, "usage").flatMap(readUsage)
            model <- str(o, "model")
          } yield Message.Assistant(blocks, stop, usage, model)
        case "tool_result" =>
          for {
            callId <- str(o, "callId")
            content <- str(o, "content")
            isError <- bool(o, "isError")
          } yield Message.ToolResult(ToolCallId(callId), content, isError)
        case other => Left(s"unknown message role: $other")
      }
    } yield m

  private def readBlock(v: ujson.Value): Either[String, AssistantBlock] =
    for {
      o <- obj(v)
      tpe <- str(o, "type")
      b <- tpe match {
        case "text" => str(o, "text").map(AssistantBlock.Text(_))
        case "reasoning" =>
          str(o, "text").map(AssistantBlock.Reasoning(_, o.value.get("replay")))
        case "tool_call" =>
          for {
            id <- str(o, "id")
            name <- str(o, "name")
            arguments <- field(o, "arguments")
          } yield AssistantBlock.ToolCall(ToolCallId(id), name, arguments)
        case other => Left(s"unknown block type: $other")
      }
    } yield b

  private def readStop(o: ujson.Obj): Either[String, StopReason] =
    str(o, "stop").flatMap {
      case "end_turn" => Right(StopReason.EndTurn)
      case "tool_use" => Right(StopReason.ToolUse)
      case "max_tokens" => Right(StopReason.MaxTokens)
      case "content_filter" => Right(StopReason.ContentFilter)
      case "other" => str(o, "stopRaw").map(StopReason.Other(_))
      case other => Left(s"unknown stop reason: $other")
    }

  private def readUsage(v: ujson.Value): Either[String, Usage] =
    for {
      o <- obj(v)
      input <- long(o, "input")
      output <- long(o, "output")
      cached <- long(o, "cachedInput")
      cost <- o.value.get("costUsd") match {
        case None => Right(None)
        case Some(ujson.Str(s)) =>
          scala.util.Try(BigDecimal(s)).toOption.map(Some(_)).toRight(s"bad costUsd: $s")
        case Some(_) => Left("costUsd is not a string")
      }
    } yield Usage(Tokens(input), Tokens(output), Tokens(cached), cost)

  private def obj(v: ujson.Value): Either[String, ujson.Obj] = v match {
    case o: ujson.Obj => Right(o)
    case _ => Left("expected an object")
  }

  private def field(o: ujson.Obj, key: String): Either[String, ujson.Value] =
    o.value.get(key).toRight(s"missing field: $key")

  private def str(o: ujson.Obj, key: String): Either[String, String] =
    field(o, key).flatMap {
      case ujson.Str(s) => Right(s)
      case _ => Left(s"$key is not a string")
    }

  private def bool(o: ujson.Obj, key: String): Either[String, Boolean] =
    field(o, key).flatMap {
      case ujson.Bool(b) => Right(b)
      case _ => Left(s"$key is not a boolean")
    }

  private def long(o: ujson.Obj, key: String): Either[String, Long] =
    field(o, key).flatMap {
      case ujson.Num(n) if n.isWhole && n >= 0 => Right(n.toLong)
      case _ => Left(s"$key is not a non-negative whole number")
    }

  private def arr(o: ujson.Obj, key: String): Either[String, Vector[ujson.Value]] =
    field(o, key).flatMap {
      case ujson.Arr(items) => Right(items.toVector)
      case _ => Left(s"$key is not an array")
    }

  private def traverse[A, B](as: Vector[A])(f: A => Either[String, B]): Either[String, Vector[B]] =
    as.foldLeft[Either[String, Vector[B]]](Right(Vector.empty)) { (acc, a) =>
      acc.flatMap(bs => f(a).map(bs :+ _))
    }
}
