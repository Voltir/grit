package grit.models

import scala.collection.mutable

import grit.core.provider.{Delta, ProviderError}

/** Anthropic's streamed message, server-sent events, folded back into the body a plain call
  * returns, so [[AnthropicJson.response]] reads both. Pure but for `onDelta`. Checked against
  * platform.claude.com ("Streaming messages") on 2026-10-29:
  *
  *   - `message_start` carries the message with empty content, and its first usage;
  *   - each block is a `content_block_start` (the block, its text or thinking empty), its
  *     `content_block_delta`s (`text_delta`, `thinking_delta`, `signature_delta`, and a tool
  *     call's arguments as `input_json_delta` pieces of one JSON text), and a
  *     `content_block_stop`;
  *   - `message_delta` carries the stop reason and its details, and usage, cumulative;
  *     `message_stop` ends the stream;
  *   - `ping` events may come anywhere, an `error` event can arrive after the 200, and new
  *     event types may be added.
  */
object AnthropicStream {

  /** The response body the SSE `lines` add up to, telling `onDelta` each piece of text and
    * thinking as it is read, and each tool call as its block begins; or why they add up to
    * none. An `error` event is [[AnthropicJson.modelError]]'s; a stream that ends before
    * `message_stop` is [[ProviderError.Unavailable]]; an unknown event is skipped.
    */
  def fold(lines: Iterator[String], onDelta: Delta => Unit): Either[ProviderError, ujson.Value] = {
    var message: Option[ujson.Obj] = None
    val blocks = mutable.LinkedHashMap.empty[Int, ujson.Obj]
    val arguments = mutable.Map.empty[Int, StringBuilder]
    var outcome: Option[Either[ProviderError, Unit]] = None

    def append(block: ujson.Obj, key: String, more: String): Unit =
      block(key) = block.value.get(key).flatMap(_.strOpt).getOrElse("") + more

    while (outcome.isEmpty && lines.hasNext) {
      val line = lines.next()
      // `event:` lines repeat the data's own `type`; blank lines separate events.
      if (line.startsWith("data:")) {
        scala.util.Try(ujson.read(line.drop(5).trim)).toOption.flatMap(_.objOpt) match {
          case None => outcome = Some(Left(unreadable("an event is not a JSON object")))
          case Some(event) =>
            def str(v: Option[ujson.Value]) = v.flatMap(_.strOpt)
            val index = event.get("index").flatMap(_.numOpt).map(_.toInt)
            str(event.get("type")) match {
              case Some("message_start") =>
                message = event.get("message").flatMap(_.objOpt).map(ujson.Obj.from(_))
              case Some("content_block_start") =>
                for {
                  i <- index
                  block <- event.get("content_block").flatMap(_.objOpt)
                } {
                  val started = ujson.Obj.from(block)
                  blocks(i) = started
                  if (str(started.value.get("type")).contains("tool_use")) {
                    arguments(i) = new StringBuilder
                    str(started.value.get("name")).foreach(n => onDelta(Delta.Calling(n)))
                  }
                }
              case Some("content_block_delta") =>
                for {
                  i <- index
                  block <- blocks.get(i)
                  delta <- event.get("delta").flatMap(_.objOpt)
                } {
                  def piece(key: String) = str(delta.get(key)).getOrElse("")
                  str(delta.get("type")) match {
                    case Some("text_delta") =>
                      append(block, "text", piece("text"))
                      onDelta(Delta.Text(piece("text")))
                    case Some("thinking_delta") =>
                      append(block, "thinking", piece("thinking"))
                      onDelta(Delta.Reasoning(piece("thinking")))
                    case Some("signature_delta") => block("signature") = piece("signature")
                    case Some("input_json_delta") =>
                      arguments.get(i).foreach(_ ++= piece("partial_json"))
                    case _ => ()
                  }
                }
              case Some("content_block_stop") =>
                for {
                  i <- index
                  block <- blocks.get(i)
                  json <- arguments.get(i)
                } block("input") = ToolArguments.read(Some(ujson.Str(json.toString)))
              case Some("message_delta") =>
                message.foreach { m =>
                  event.get("delta").flatMap(_.objOpt).foreach(_.foreach(m(_) = _))
                  event.get("usage").flatMap(_.objOpt).foreach { counts =>
                    val usage =
                      m.value.get("usage").flatMap(_.objOpt).fold(ujson.Obj())(ujson.Obj.from(_))
                    counts.foreach(usage(_) = _)
                    m("usage") = usage
                  }
                }
              case Some("message_stop") => outcome = Some(Right(()))
              case Some("error") =>
                outcome = Some(
                  Left(
                    AnthropicJson.modelError(
                      event.get("error").flatMap(_.objOpt).getOrElse(mutable.Map.empty)
                    )
                  )
                )
              case _ => () // `ping`, and event types added after this was written
            }
        }
      }
    }

    // A stream cut short is a connection lost, which another try may get past.
    val cut = ProviderError.Unavailable("unreadable stream: the stream ended before message_stop")
    outcome.getOrElse(Left(cut)).flatMap { _ =>
      message.toRight(unreadable("no message_start")).map { m =>
        m("content") = ujson.Arr.from(blocks.values)
        m
      }
    }
  }

  private def unreadable(why: String): ProviderError =
    ProviderError.Refused(s"unreadable stream: $why")
}
