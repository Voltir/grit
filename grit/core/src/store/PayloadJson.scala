package grit.core.store

import grit.core.id.{ConversationId, EntryId, PeriodSeq, ToolCallId, TurnSeq}
import grit.core.message.{AssistantBlock, Message, StopReason, Tokens, Usage}
import grit.core.period.{CloseReason, ClosingJson, Probability}
import grit.core.place.Place
import grit.core.topic.TopicJson

/** The stored JSON form of a [[Payload]]. Written by hand, not derived: it is
  * persisted data, so a rename in Scala must not change it, and reading it is
  * total. (upickle's derivation also cannot see through capture checking's
  * inferred-type annotations.)
  */
object PayloadJson {

  def write(p: Payload): ujson.Value = p match {
    case Payload.Message(m) => ujson.Obj("kind" -> "message", "message" -> message(m))
    case Payload.Heard(text) => ujson.Obj("kind" -> "heard", "text" -> text)
    case Payload.Summary(text) => ujson.Obj("kind" -> "summary", "text" -> text)
    case Payload.Query(text) => ujson.Obj("kind" -> "query", "text" -> text)
    case Payload.Window(entries, recalled, nearby) =>
      val o = ujson.Obj(
        "kind" -> "window",
        "entries" -> ujson.Arr.from(entries.map(e => ujson.Str(EntryId.value(e)))),
        "recalled" -> ujson.Arr.from(recalled.map(t => ujson.Num(TurnSeq.value(t).toDouble)))
      )
      // Written only when there are sections, so a window without any keeps the form
      // every earlier build wrote.
      if (nearby.nonEmpty) o("nearby") = ujson.Arr.from(nearby.map(writeNearby))
      o
    case Payload.Topic(events) =>
      ujson.Obj("kind" -> "topic", "events" -> ujson.Arr.from(events.map(TopicJson.write)))
    case Payload.Exchange(reply) => ujson.Obj("kind" -> "exchange", "message" -> message(reply))
    case Payload.Result(result, shown) =>
      ujson.Obj("kind" -> "exchange", "message" -> message(result), "shown" -> shown)
    case Payload.Attempt(call) => ujson.Obj("kind" -> "attempt", "call" -> ToolCallId.value(call))
    case Payload.Ask(call, shown) =>
      ujson.Obj("kind" -> "ask", "call" -> ToolCallId.value(call), "shown" -> shown)
    case Payload.Closed(period, reason, closing) =>
      val o = ujson.Obj(
        "kind" -> "closed",
        "period" -> PeriodSeq.value(period).toDouble,
        "reason" -> reasonName(reason)
      )
      reasonConfidence(reason).foreach(c => o("confidence") = c)
      o("closing") = ClosingJson.write(closing)
      o
  }

  /** The stored name of a [[CloseReason]]: the same in a closing entry, a period's row and a
    * close's journal.
    */
  def reasonName(reason: CloseReason): String = reason match {
    case CloseReason.Resolved(_) => "resolved"
    case CloseReason.Lapsed => "lapsed"
    case CloseReason.Unearned => "unearned"
  }

  /** A resolved reason's confidence, stored beside its name; `None` for any other. */
  def reasonConfidence(reason: CloseReason): Option[Double] = reason match {
    case CloseReason.Resolved(c) => Some(Probability.value(c))
    case CloseReason.Lapsed | CloseReason.Unearned => None
  }

  /** The [[CloseReason]] stored as `name` with `confidence`, or why it is none: an unknown
    * name, a resolved one without a confidence in [0, 1], or a lapsed or unearned one with a
    * confidence.
    */
  def readReason(name: String, confidence: Option[Double]): Either[String, CloseReason] =
    (name, confidence) match {
      case ("resolved", Some(c)) =>
        Probability
          .of(c)
          .map(CloseReason.Resolved(_))
          .toRight(s"confidence $c is not a probability")
      case ("resolved", None) => Left("a resolved close has no confidence")
      case ("lapsed", None) => Right(CloseReason.Lapsed)
      case ("lapsed", Some(_)) => Left("a lapsed close has a confidence")
      case ("unearned", None) => Right(CloseReason.Unearned)
      case ("unearned", Some(_)) => Left("an unearned close has a confidence")
      case (other, _) => Left(s"unknown close reason: $other")
    }

  /** The payload `v` encodes, or why it encodes none. An [[Payload.Exchange]] and a
    * [[Payload.Result]] share the kind `exchange`, told apart by their message's role, so
    * rows written before they were two cases read as before. A result kept before calls
    * were shown has no `shown`, and reads with its call's id in its place.
    */
  def read(v: ujson.Value): Either[String, Payload] =
    for {
      o <- obj(v)
      kind <- str(o, "kind")
      p <- kind match {
        case "message" => field(o, "message").flatMap(readMessage).map(Payload.Message(_))
        case "heard" => str(o, "text").map(Payload.Heard(_))
        case "summary" => str(o, "text").map(Payload.Summary(_))
        case "query" => str(o, "text").map(Payload.Query(_))
        case "window" =>
          for {
            entries <- arr(o, "entries").flatMap(traverse(_) {
              case ujson.Str(id) => Right(EntryId(id))
              case _ => Left("an entry id is not a string")
            })
            recalled <- arr(o, "recalled").flatMap(traverse(_) {
              case ujson.Num(n) if n.isWhole && n >= 0 => Right(TurnSeq(n.toLong))
              case _ => Left("a recalled turn is not a non-negative whole number")
            })
            nearby <-
              if (o.value.contains("nearby")) arr(o, "nearby").flatMap(traverse(_)(readNearby))
              else Right(Vector.empty)
          } yield Payload.Window(entries, recalled, nearby)
        case "topic" => arr(o, "events").flatMap(traverse(_)(TopicJson.read)).map(Payload.Topic(_))
        case "exchange" =>
          field(o, "message").flatMap(readMessage).flatMap {
            case reply: Message.Assistant => Right(Payload.Exchange(reply))
            case result: Message.ToolResult =>
              o.value.get("shown") match {
                case None => Right(Payload.Result(result, ToolCallId.value(result.callId)))
                case Some(ujson.Str(shown)) => Right(Payload.Result(result, shown))
                case Some(_) => Left("shown is not a string")
              }
            case Message.User(_) => Left("an exchange holds no user message")
          }
        case "attempt" => str(o, "call").map(c => Payload.Attempt(ToolCallId(c)))
        case "ask" =>
          for {
            call <- str(o, "call")
            shown <- str(o, "shown")
          } yield Payload.Ask(ToolCallId(call), shown)
        case "closed" =>
          for {
            n <- long(o, "period")
            period <- PeriodSeq.of(n).toRight(s"period $n is below the first")
            name <- str(o, "reason")
            confidence <- o.value.get("confidence") match {
              case None => Right(None)
              case Some(ujson.Num(c)) => Right(Some(c))
              case Some(_) => Left("confidence is not a number")
            }
            reason <- readReason(name, confidence)
            closing <- field(o, "closing").flatMap(ClosingJson.read)
          } yield Payload.Closed(period, reason, closing)
        case other => Left(s"unknown payload kind: $other")
      }
    } yield p

  private def message(m: Message): ujson.Value = m match {
    case Message.User(text) => ujson.Obj("role" -> "user", "text" -> text)
    case Message.Assistant(blocks, stop, usage, model, upstream) =>
      val base = ujson.Obj(
        "role" -> "assistant",
        "blocks" -> ujson.Arr.from(blocks.map(block)),
        "usage" -> writeUsage(usage),
        "model" -> model
      )
      upstream.foreach(u => base("upstream") = u)
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

  /** A [[Usage]] in the stored form; the cost is a string so the provider's decimal
    * survives exactly.
    */
  def writeUsage(u: Usage): ujson.Value = {
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
            upstream <- o.value.get("upstream") match {
              case None => Right(None)
              case Some(u) => u.strOpt.map(Some(_)).toRight("upstream is not text")
            }
          } yield Message.Assistant(blocks, stop, usage, model, upstream)
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

  /** The [[Usage]] `v` encodes in [[writeUsage]]'s form, or why it encodes none. */
  def readUsage(v: ujson.Value): Either[String, Usage] =
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

  /** A nearby section's stored form: its conversation, its place as written, and an open
    * one's `entries` or a closed one's `closing`.
    */
  def writeNearby(n: Nearby): ujson.Value =
    n match {
      case Nearby.Open(c, place, entries) =>
        ujson.Obj(
          "conversation" -> ConversationId.value(c),
          "place" -> place.written,
          "entries" -> ujson.Arr.from(entries.map(e => ujson.Str(EntryId.value(e))))
        )
      case Nearby.Closed(c, place, closing) =>
        ujson.Obj(
          "conversation" -> ConversationId.value(c),
          "place" -> place.written,
          "closing" -> EntryId.value(closing)
        )
    }

  /** The nearby section `v` stores ([[writeNearby]]'s form), or why none. */
  def readNearby(v: ujson.Value): Either[String, Nearby] =
    for {
      o <- obj(v)
      c <- str(o, "conversation")
      written <- str(o, "place")
      place <- Place.read(written)
      section <-
        if (o.value.contains("closing"))
          str(o, "closing").map(k => Nearby.Closed(ConversationId(c), place, EntryId(k)))
        else
          arr(o, "entries")
            .flatMap(traverse(_) {
              case ujson.Str(id) => Right(EntryId(id))
              case _ => Left("an entry id is not a string")
            })
            .map(Nearby.Open(ConversationId(c), place, _))
    } yield section

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
