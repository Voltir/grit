package grit.mcp.wire

/** What JSON-RPC carries for grit, at revision [[Rpc.Version]]. */
object Rpc {

  /** `2026-07-28`, the only revision grit speaks. */
  val Version: String = "2026-07-28"

  /** A request grit sends, and its method. */
  enum Call(val method: String) {

    /** `tools/list`: the first page, or the page `cursor` names. */
    case ListTools(cursor: Option[String]) extends Call("tools/list")

    /** `tools/call`: `tool` run with `arguments`. */
    case CallTool(tool: McpTool, arguments: ujson.Obj) extends Call("tools/call")
  }

  /** Request `id` making `call`, its params' `_meta` declaring [[Version]], no client
    * capabilities, and grit as the client (`{"name": "grit", "version": "0.1.0"}`).
    */
  def request(id: Long, call: Call): ujson.Obj = {
    val params = call match {
      case Call.ListTools(cursor) => ujson.Obj.from(cursor.map(c => "cursor" -> ujson.Str(c)))
      case Call.CallTool(tool, arguments) =>
        ujson.Obj("name" -> tool.name, "arguments" -> arguments)
    }
    params("_meta") = ujson.Obj(
      "io.modelcontextprotocol/protocolVersion" -> Version,
      "io.modelcontextprotocol/clientCapabilities" -> ujson.Obj(),
      "io.modelcontextprotocol/clientInfo" -> ujson.Obj("name" -> "grit", "version" -> "0.1.0")
    )
    ujson.Obj("jsonrpc" -> "2.0", "id" -> id.toDouble, "method" -> call.method, "params" -> params)
  }

  /** The result of request `id` in `message`, a JSON-RPC response whose `resultType` is
    * absent or `complete`; else [[McpError.InputRequired]] for `input_required`,
    * [[McpError.Unsupported]] or [[McpError.Rpc]] for an error (one whose id could not be read
    * included), and [[McpError.Unreadable]] for an unknown `resultType`, an answer to another
    * request, or anything that is not a response.
    */
  def result(id: Long, message: ujson.Value): Either[McpError, ujson.Obj] =
    message.objOpt.toRight(McpError.Unreadable("not a JSON object")).flatMap { o =>
      val answers = o.get("id")
      def to(response: => Either[McpError, ujson.Obj]): Either[McpError, ujson.Obj] =
        answers match {
          case Some(ujson.Num(n)) if n == id.toDouble => response
          case Some(other) =>
            Left(McpError.Unreadable(s"a response to request ${ujson.write(other)}, not $id"))
          case None => Left(McpError.Unreadable(s"a response to no request, not $id"))
        }
      (o.get("result"), o.get("error")) match {
        case (Some(r), _) =>
          to(
            r.objOpt
              .toRight(McpError.Unreadable("a result that is not an object"))
              .flatMap(complete)
          )
        case (None, Some(e)) if answers.forall(_ == ujson.Null) => Left(error(e))
        case (None, Some(e)) => to(Left(error(e)))
        case (None, None) => Left(McpError.Unreadable("neither a result nor an error"))
      }
    }

  /** `result` when its `resultType` is absent or `complete`. */
  private def complete(result: collection.Map[String, ujson.Value]): Either[McpError, ujson.Obj] =
    result.get("resultType") match {
      case None | Some(ujson.Str("complete")) => Right(ujson.Obj.from(result))
      case Some(ujson.Str("input_required")) => Left(McpError.InputRequired)
      case Some(ujson.Str(other)) => Left(McpError.Unreadable(s"an unknown resultType: $other"))
      case Some(other) =>
        Left(McpError.Unreadable(s"a resultType that is not a string: ${ujson.write(other)}"))
    }

  /** The error an HTTP `status` other than 200 is, its body `body`: with 401
    * [[McpError.Unauthorized]] and with 403 [[McpError.Forbidden]], each keeping `challenge`
    * (the response's `WWW-Authenticate`); else the JSON-RPC error in `body`, as
    * [[result]] reads one; else, with no error in `body`, [[McpError.Rejected]] for a 4xx,
    * keeping `body` as one line (each run of whitespace or control characters a space, at most
    * [[Excerpt]] characters, then "…"), and [[McpError.Status]] for any other status.
    */
  def failure(status: Int, body: String, challenge: Option[String]): McpError =
    status match {
      case 401 => McpError.Unauthorized(challenge)
      case 403 => McpError.Forbidden(challenge)
      case _ =>
        scala.util.Try(ujson.read(body)).toOption.flatMap(_.objOpt).flatMap(_.get("error")) match {
          case Some(e) => error(e)
          case None if status >= 400 && status < 500 => McpError.Rejected(status, line(body))
          case None => McpError.Status(status)
        }
    }

  /** The most characters of a body [[failure]] keeps: 200. */
  val Excerpt: Int = 200

  /** `body` as one line of at most [[Excerpt]] characters, then "…". */
  private def line(body: String): String = {
    val flat = body
      .map(c => if (c.isWhitespace || c.isControl) ' ' else c)
      .trim
      .replaceAll(" {2,}", " ")
    if (flat.length > Excerpt) flat.take(Excerpt) + "…" else flat
  }

  private val UnsupportedProtocolVersion = -32022

  private def error(e: ujson.Value): McpError = {
    val o = e.objOpt
    val code = o.flatMap(_.get("code")).flatMap(_.numOpt).filter(_.isWhole).map(_.toInt)
    (code, o.flatMap(_.get("message")).flatMap(_.strOpt)) match {
      case (Some(UnsupportedProtocolVersion), _) =>
        McpError.Unsupported(
          o.flatMap(_.get("data"))
            .flatMap(_.objOpt)
            .flatMap(_.get("supported"))
            .flatMap(_.arrOpt)
            .fold(Vector.empty[String])(_.toVector.flatMap(_.strOpt))
        )
      case (Some(code), Some(said)) => McpError.Rpc(code, said)
      case _ => McpError.Unreadable("an error with no code and message")
    }
  }
}
