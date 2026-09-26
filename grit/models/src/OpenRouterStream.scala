package grit.models

import scala.collection.mutable

import grit.core.provider.{Delta, ProviderError}

/** OpenRouter's streamed chat completion, server-sent events, folded back into the body a
  * non-streaming call returns, so [[OpenRouterJson.response]] reads both. Pure but for
  * `onDelta`. Checked against openrouter.ai/docs (streaming, reasoning tokens) and a
  * captured stream (`test/resources/openrouter-stream.sse`) on 2026-09-24:
  *
  *   - `:` lines are comments; `data: [DONE]` ends the stream;
  *   - each `data:` chunk's `choices[0].delta` carries `content`, `reasoning` and
  *     `reasoning_details` pieces, the details indexed so the pieces of one merge;
  *   - `finish_reason` and `usage` arrive in the last chunks; an error after the 200 is a
  *     chunk with a top-level `error`;
  *   - `delta.tool_calls` pieces are indexed like the details: the first of a call carries
  *     its `id`, `type` and `function.name`, later ones more of `function.arguments`
  *     (`test/resources/openrouter-toolcall.sse`, google/gemini-2.5-flash-lite,
  *     2026-09-24). A call can come with text in the same response, and `finish_reason`
  *     can say `stop` rather than `tool_calls`: a call is known by being there.
  */
object OpenRouterStream {

  /** The response body the SSE `lines` add up to, telling `onDelta` each piece of text and
    * reasoning as it is read, and each tool call once its name has arrived; or why they add
    * up to none.
    */
  def fold(lines: Iterator[String], onDelta: Delta => Unit): Either[ProviderError, ujson.Value] = {
    val content = new StringBuilder
    val reasoning = new StringBuilder
    val details = mutable.LinkedHashMap.empty[Int, ujson.Obj]
    val calls = mutable.LinkedHashMap.empty[Int, ujson.Obj]
    var finish: Option[String] = None
    var usage: Option[ujson.Value] = None
    var model: Option[String] = None
    var provider: Option[String] = None
    var outcome: Option[Either[ProviderError, Unit]] = None

    while (outcome.isEmpty && lines.hasNext) {
      val line = lines.next()
      if (line.startsWith("data:")) {
        val data = line.drop(5).trim
        if (data == "[DONE]") outcome = Some(Right(()))
        else
          scala.util.Try(ujson.read(data)).toOption.flatMap(_.objOpt) match {
            case None => outcome = Some(Left(unreadable("a chunk is not a JSON object")))
            case Some(chunk) =>
              chunk.get("error").flatMap(_.objOpt) match {
                case Some(error) =>
                  outcome = Some(Left(OpenRouterJson.modelError(error)))
                case None =>
                  chunk.get("model").flatMap(_.strOpt).foreach(m => model = Some(m))
                  chunk.get("provider").flatMap(_.strOpt).foreach(p => provider = Some(p))
                  chunk.get("usage").filter(_.objOpt.isDefined).foreach(u => usage = Some(u))
                  val choice = chunk
                    .get("choices")
                    .flatMap(_.arrOpt)
                    .flatMap(_.headOption)
                    .flatMap(_.objOpt)
                  choice.flatMap(_.get("finish_reason")).flatMap(_.strOpt).foreach { f =>
                    finish = Some(f)
                  }
                  choice.flatMap(_.get("delta")).flatMap(_.objOpt).foreach { delta =>
                    val pieces =
                      delta.get("reasoning_details").flatMap(_.arrOpt).getOrElse(Seq.empty)
                    pieces.flatMap(_.objOpt).foreach(d => merge(details, d))
                    // The plain `reasoning` string when sent; else the details' text pieces.
                    val thought = delta
                      .get("reasoning")
                      .flatMap(_.strOpt)
                      .getOrElse(
                        pieces.flatMap(_.objOpt).flatMap(_.get("text")).flatMap(_.strOpt).mkString
                      )
                    if (thought.nonEmpty) {
                      reasoning ++= thought
                      onDelta(Delta.Reasoning(thought))
                    }
                    delta
                      .get("tool_calls")
                      .flatMap(_.arrOpt)
                      .getOrElse(Seq.empty)
                      .flatMap(_.objOpt)
                      .foreach(c => mergeCall(calls, c).foreach(n => onDelta(Delta.Calling(n))))
                    delta.get("content").flatMap(_.strOpt).filter(_.nonEmpty).foreach { text =>
                      content ++= text
                      onDelta(Delta.Text(text))
                    }
                  }
              }
          }
      }
      // Blank lines separate events; `:` lines are comments.
    }

    // A stream cut short is a connection lost, which another try may get past.
    val cut = ProviderError.Unavailable("unreadable stream: the stream ended before [DONE]")
    outcome.getOrElse(Left(cut)).map { _ =>
      val message = ujson.Obj("role" -> "assistant", "content" -> content.toString)
      if (reasoning.nonEmpty) message("reasoning") = reasoning.toString
      if (details.nonEmpty) message("reasoning_details") = ujson.Arr.from(details.values)
      if (calls.nonEmpty) message("tool_calls") = ujson.Arr.from(calls.values)
      val body = ujson.Obj(
        "model" -> model.getOrElse("unknown"),
        "choices" -> ujson.Arr(
          ujson.Obj(
            "message" -> message,
            "finish_reason" -> finish.fold[ujson.Value](ujson.Null)(ujson.Str(_))
          )
        )
      )
      usage.foreach(u => body("usage") = u)
      provider.foreach(p => body("provider") = p)
      body
    }
  }

  /** `piece` merged into the detail at its index: its text, summary or data appended, any
    * other field the first piece to name it sets.
    */
  private def merge(details: mutable.LinkedHashMap[Int, ujson.Obj], piece: ujson.Obj): Unit = {
    val index = piece.value.get("index").flatMap(_.numOpt).fold(details.size)(_.toInt)
    details.get(index) match {
      case None => details(index) = ujson.Obj.from(piece.value)
      case Some(detail) =>
        piece.value.foreach {
          case (key @ ("text" | "summary" | "data"), ujson.Str(more)) =>
            detail(key) = detail.value.get(key).flatMap(_.strOpt).getOrElse("") + more
          case (key, value) => if (!detail.value.contains(key)) detail(key) = value
        }
    }
  }

  /** `piece` of a tool call merged into the call at its index: more of its
    * `function.arguments` appended, any other field the first piece to name it sets. The
    * call's name, when this piece is the one that set it.
    */
  private def mergeCall(
      calls: mutable.LinkedHashMap[Int, ujson.Obj],
      piece: ujson.Obj
  ): Option[String] = {
    val index = piece.value.get("index").flatMap(_.numOpt).fold(calls.size)(_.toInt)
    val call = calls.getOrElseUpdate(index, ujson.Obj())
    var named: Option[String] = None
    piece.value.foreach {
      case ("index", _) => ()
      case ("function", f: ujson.Obj) =>
        val function = call.value.get("function").flatMap(_.objOpt) match {
          case Some(_) => call("function").obj
          case None =>
            call("function") = ujson.Obj()
            call("function").obj
        }
        f.value.foreach {
          case ("arguments", ujson.Str(more)) =>
            function("arguments") = function.get("arguments").flatMap(_.strOpt).getOrElse("") + more
          case (key, value) =>
            if (!function.contains(key)) {
              function(key) = value
              if (key == "name") named = value.strOpt
            }
        }
      case (key, value) => if (!call.value.contains(key)) call(key) = value
    }
    named
  }

  private def unreadable(why: String): ProviderError =
    ProviderError.Refused(s"unreadable stream: $why")
}
