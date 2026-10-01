package grit.mcp.wire

/** A response sent as Server-Sent Events. */
object Sse {

  /** The JSON-RPC response to request `id` in `lines`, an event stream: an event's data lines
    * joined by newlines; comments, notifications and responses to other requests passed
    * over, and nothing after the response read. [[McpError.Unreachable]] when the stream
    * ends first (in the middle of an event included), [[McpError.Unreadable]] when an
    * event's data is not JSON.
    */
  def response(lines: Iterator[String], id: Long): Either[McpError, ujson.Value] = {
    // The data lines of the event being read; an event ends at a blank line.
    val data = Vector.newBuilder[String]
    var pending = false
    var found: Option[Either[McpError, ujson.Value]] = None
    while (found.isEmpty && lines.hasNext) {
      val line = lines.next().stripSuffix("\r")
      if (line.isEmpty) {
        val event = data.result().mkString("\n")
        data.clear()
        if (pending) found = message(event, id)
        pending = false
      } else if (line.startsWith("data:")) {
        data += line.drop(5).stripPrefix(" ")
        pending = true
      }
      // A comment (a line starting with ':') and every other field carry no data.
    }
    found.getOrElse(Left(McpError.Unreachable("the event stream ended before the response")))
  }

  /** `event` when it is the response to `id` (or an error with no id); `None` when it is
    * another message.
    */
  private def message(event: String, id: Long): Option[Either[McpError, ujson.Value]] =
    scala.util.Try(ujson.read(event)).toOption match {
      case None => Some(Left(McpError.Unreadable(s"an event's data is not JSON: $event")))
      case Some(v) =>
        val o = v.objOpt
        val answers = o.exists(m => m.contains("result") || m.contains("error"))
        val to = o.flatMap(_.get("id"))
        val ours = to.forall(_ == ujson.Null) || to.contains(ujson.Num(id.toDouble))
        Option.when(answers && ours)(Right(v))
    }
}
