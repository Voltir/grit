package grit.models

import grit.core.message.StopReason

/** A tool call's arguments as a provider sent them, read into the form
  * [[grit.core.message.AssistantBlock.ToolCall]] holds.
  */
private[models] object ToolArguments {

  /** `sent` read as JSON when it is a string, taken at its word when it is already an object;
    * absent or blank arguments, or any other value, are `{}`. Text that does not read as JSON
    * is read again with each comma before a close dropped and, only when the reply `stop`ped
    * at [[StopReason.ToolUse]], its open brackets closed; when that is still no object, it is
    * kept whole as a `ujson.Str`.
    */
  def read(sent: Option[ujson.Value], stop: StopReason): ujson.Value = sent match {
    case Some(ujson.Str(raw)) if raw.trim.nonEmpty =>
      parsed(raw)
        .orElse(repaired(raw, closing = stop == StopReason.ToolUse).flatMap(parsed).collect {
          case o: ujson.Obj => o
        })
        // Not JSON: kept whole, for the loop's reader to refuse and echo.
        .getOrElse(ujson.Str(raw))
    case Some(o: ujson.Obj) => o
    case _ => ujson.Obj()
  }

  private def parsed(raw: String): Option[ujson.Value] = scala.util.Try(ujson.read(raw)).toOption

  /** `raw` with each comma before a close dropped and, when `closing`, its open brackets
    * closed; `None` when it ends inside a string, closes a bracket it did not open, or leaves
    * one open while not `closing`. A cut string is never closed: its value is not what was
    * meant.
    */
  private def repaired(raw: String, closing: Boolean): Option[String] = {
    val out = new StringBuilder
    var open = List.empty[Char]
    var inString = false
    var escaped = false
    var broken = false
    def dropComma(): Unit = {
      var end = out.length
      while (end > 0 && out(end - 1).isWhitespace) end -= 1
      if (end > 0 && out(end - 1) == ',') out.delete(end - 1, end)
    }
    raw.foreach { c =>
      if (inString) {
        if (escaped) escaped = false
        else if (c == '\\') escaped = true
        else if (c == '"') inString = false
        out += c
      } else
        c match {
          case '"' => inString = true; out += c
          case '{' => open = '}' :: open; out += c
          case '[' => open = ']' :: open; out += c
          case '}' | ']' =>
            if (open.headOption.contains(c)) open = open.drop(1) else broken = true
            dropComma()
            out += c
          case _ => out += c
        }
    }
    if (broken || inString || (open.nonEmpty && !closing)) None
    else {
      if (open.nonEmpty) dropComma()
      Some(out.result() + open.mkString)
    }
  }
}
