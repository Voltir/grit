package grit.mcp.wire

import grit.edge.http.{Request, Response, Status, Verb}

/** What JSON-RPC carries for grit, at revision [[Rpc.Version]]. */
object Rpc {

  /** `2026-07-28`, the only revision grit speaks. */
  val Version: String = Revision.Jul2026.text

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

  /** The response grit sends to a request a server sent it, `id`: error -32601, Method not
    * found, since grit takes no request from a server.
    */
  def refusal(id: ujson.Value): ujson.Obj =
    ujson.Obj(
      "jsonrpc" -> "2.0",
      "id" -> id,
      "error" -> ujson.Obj(
        "code" -> Code.MethodNotFound,
        "message" -> "grit takes no request from a server"
      )
    )

  /** `request`, posted to an MCP endpoint, read as one JSON-RPC message in the revision it
    * declares (the module's README, "Serving"). Only `tools/list` and `tools/call` are left to
    * the server; anything else is [[Asked.Answered]]: `server/discover`, `initialize` (in the
    * revision asked for when it is 2025-06-18 or 2025-11-25, else 2025-11-25) and `ping` from
    * `server`; a notification or a client's response, 202 with no body; a verb other than POST,
    * 405; and, with the error and status its revision gives, a body that is not one JSON-RPC 2.0
    * request, headers that disagree with it, a `_meta` lacking a field in 2026-07-28, a revision
    * not listed, an unknown method, and a `tools/list` or `tools/call` whose params cannot be
    * read. `Origin`, `Authorization`, `Mcp-Session-Id` and `Mcp-Param-*` are not read.
    */
  def received(request: Request, server: ServerInfo): Asked =
    if (request.verb != Verb.Post)
      Asked.Answered(
        Response.text(Status.NotAllowed(Vector(Verb.Post)), "post one JSON-RPC message")
      )
    else
      message(request.body) match {
        case Left(refusal) => Asked.Answered(refused(refusal, Status.BadRequest))
        case Right(Message.Notice) => Asked.Answered(Response.text(Status.Accepted, ""))
        case Right(m: Message.Call) =>
          val meta = m.params.get("_meta").flatMap(_.objOpt)
          if (
            meta.exists(_.contains(VersionKey)) ||
            request.header("MCP-Protocol-Version").contains(Revision.Jul2026.text)
          ) stateless(request, m, meta, server)
          else older(request, m, server)
      }

  /** A message [[received]] read: a request, or one answered 202 (a notification, or a client's
    * response).
    */
  private enum Message {
    case Call(id: ujson.Value, method: String, params: collection.Map[String, ujson.Value])
    case Notice
  }

  /** A JSON-RPC error answering the request `id`, `ujson.Null` when it could not be read. */
  private[wire] final case class Refusal(
      id: ujson.Value,
      code: Int,
      text: String,
      data: Option[ujson.Value] = None
  )

  private val VersionKey = "io.modelcontextprotocol/protocolVersion"
  private val CapabilitiesKey = "io.modelcontextprotocol/clientCapabilities"
  private val ServerInfoKey = "io.modelcontextprotocol/serverInfo"

  /** The largest integer a double holds exactly, 2^53^−1: the largest id read as sent. */
  private val SafeId = 9007199254740991d

  /** `body` as one JSON-RPC 2.0 message, apart from what its revision says of it. */
  private def message(body: String): Either[Refusal, Message] =
    scala.util.Try(ujson.read(body)).toOption match {
      case None => Left(Refusal(ujson.Null, Code.ParseError, "the body is not JSON"))
      case Some(_: ujson.Arr) =>
        Left(Refusal(ujson.Null, Code.InvalidRequest, "a body holds one message, never a batch"))
      case Some(o: ujson.Obj) =>
        val m = o.value
        val id = m.get("id")
        def invalid(why: String) =
          Left(Refusal(id.filter(readable).getOrElse(ujson.Null), Code.InvalidRequest, why))
        if (!m.get("jsonrpc").contains(ujson.Str("2.0")))
          invalid("a message carries \"jsonrpc\": \"2.0\"")
        else
          m.get("method") match {
            case None if m.contains("result") || m.contains("error") => Right(Message.Notice)
            case None => invalid("a request names its method")
            case Some(ujson.Str(method)) =>
              (id, m.get("params")) match {
                case (Some(i), _) if !readable(i) =>
                  invalid("an id is a string or an integer within ±(2^53−1)")
                case (_, Some(p)) if p.objOpt.isEmpty => invalid("params are a JSON object")
                case (None, _) if method.startsWith("notifications/") => Right(Message.Notice)
                case (None, _) => invalid(s"$method is a request, so it carries an id")
                case (Some(i), params) =>
                  Right(Message.Call(i, method, params.flatMap(_.objOpt).getOrElse(Map.empty)))
              }
            case Some(_) => invalid("a method is a string")
          }
      case Some(_) => Left(Refusal(ujson.Null, Code.InvalidRequest, "a message is a JSON object"))
    }

  /** Whether `id` is one an answer can carry back as sent: a string, or a whole number within
    * [[SafeId]].
    */
  private def readable(id: ujson.Value): Boolean = id match {
    case ujson.Str(_) => true
    case ujson.Num(n) => n.isWhole && n.abs <= SafeId
    case _ => false
  }

  /** `m`, a request declaring 2026-07-28 by its `_meta` or its header, held to that revision's
    * headers and `_meta` in the README's order.
    */
  private def stateless(
      request: Request,
      m: Message.Call,
      meta: Option[collection.Map[String, ujson.Value]],
      server: ServerInfo
  ): Asked = {
    val header = request.header("MCP-Protocol-Version")
    val version = meta.flatMap(_.get(VersionKey)).flatMap(_.strOpt)
    def refuse(status: Status, code: Int, text: String, data: Option[ujson.Value] = None) =
      Asked.Answered(refused(Refusal(m.id, code, text, data), status))
    (header, version) match {
      case (None, _) =>
        refuse(Status.BadRequest, Code.HeaderMismatch, "the MCP-Protocol-Version header is missing")
      case (_, None) =>
        refuse(Status.BadRequest, Code.InvalidParams, s"_meta lacks $VersionKey")
      case (Some(h), Some(v)) if h != v =>
        refuse(
          Status.BadRequest,
          Code.HeaderMismatch,
          s"MCP-Protocol-Version $h does not match _meta's $v"
        )
      case _ if !request.header("Mcp-Method").contains(m.method) =>
        refuse(Status.BadRequest, Code.HeaderMismatch, "Mcp-Method does not match the method")
      case _
          if m.method == "tools/call" &&
            request.header("Mcp-Name").map(Headers.decoded) !=
            m.params.get("name").flatMap(_.strOpt) =>
        refuse(Status.BadRequest, Code.HeaderMismatch, "Mcp-Name does not match params.name")
      case _ if !meta.flatMap(_.get(CapabilitiesKey)).exists(_.objOpt.nonEmpty) =>
        refuse(Status.BadRequest, Code.InvalidParams, s"_meta lacks $CapabilitiesKey")
      case (_, Some(v)) if v != Revision.Jul2026.text =>
        refuse(
          Status.BadRequest,
          Code.UnsupportedVersion,
          s"unsupported protocol version: $v",
          Some(unsupported(v))
        )
      case _ =>
        m.method match {
          case "server/discover" =>
            Asked.Answered(answer(m.id, Revision.Jul2026, discovered(server)))
          case "tools/list" | "tools/call" => asked(m, Revision.Jul2026)
          case other =>
            refuse(Status.NotFound, Code.MethodNotFound, s"no method is named $other")
        }
    }
  }

  /** `m`, a request declaring no 2026-07-28, in the revision its header names, 2025-06-18 when
    * it has none; `initialize` in the revision it asks for.
    */
  private def older(request: Request, m: Message.Call, server: ServerInfo): Asked = {
    def refuse(status: Status, code: Int, text: String, data: Option[ujson.Value] = None) =
      Asked.Answered(refused(Refusal(m.id, code, text, data), status))
    request.header("MCP-Protocol-Version") match {
      case Some(h) if Revision.named(h).isEmpty =>
        refuse(
          Status.BadRequest,
          Code.InvalidParams,
          s"unsupported protocol version: $h",
          Some(unsupported(h))
        )
      case header =>
        val revision = header.flatMap(Revision.named).getOrElse(Revision.Jun2025)
        m.method match {
          case "initialize" =>
            val asked = m.params.get("protocolVersion").flatMap(_.strOpt).flatMap(Revision.named)
            Asked.Answered(answer(m.id, revision, initialized(asked, server)))
          case "ping" => Asked.Answered(answer(m.id, revision, ujson.Obj()))
          case "tools/list" | "tools/call" => asked(m, revision)
          case other => refuse(Status.Ok, Code.MethodNotFound, s"no method is named $other")
        }
    }
  }

  /** `m`, a `tools/list` or a `tools/call` read in `revision`, for the server; or -32602 when its
    * params cannot be read.
    */
  private def asked(m: Message.Call, revision: Revision): Asked = {
    def invalid(why: String) =
      Asked.Answered(refused(Refusal(m.id, Code.InvalidParams, why), invalidStatus(revision)))
    if (m.method == "tools/list")
      m.params.get("cursor") match {
        case None | Some(ujson.Null) => Asked.List(None, m.id, revision)
        case Some(ujson.Str(c)) => Asked.List(Some(c), m.id, revision)
        case Some(_) => invalid("a cursor is a string")
      }
    else
      (m.params.get("name"), m.params.get("arguments")) match {
        case (Some(ujson.Str(tool)), None | Some(ujson.Null)) =>
          Asked.Call(tool, ujson.Obj(), m.id, revision)
        case (Some(ujson.Str(tool)), Some(a: ujson.Obj)) => Asked.Call(tool, a, m.id, revision)
        case (Some(ujson.Str(_)), Some(_)) => invalid("a tool's arguments are a JSON object")
        case _ => invalid("tools/call names its tool")
      }
  }

  /** The status -32602 is sent with in `revision`: 400 in 2026-07-28, 200 before. */
  private[wire] def invalidStatus(revision: Revision): Status =
    if (revision == Revision.Jul2026) Status.BadRequest else Status.Ok

  /** -32022's data, and the older era's -32602's, for a request naming `requested`. */
  private def unsupported(requested: String): ujson.Obj =
    ujson.Obj(
      "supported" -> ujson.Arr.from(Revision.Listed.map(_.text)),
      "requested" -> requested
    )

  /** The capabilities a server here declares: tools, whose list never changes while it runs. */
  private def capabilities: ujson.Obj = ujson.Obj("tools" -> ujson.Obj("listChanged" -> false))

  /** `server/discover`'s result for `server`. */
  private def discovered(server: ServerInfo): ujson.Obj = {
    val result = ujson.Obj(
      "supportedVersions" -> ujson.Arr.from(Revision.Listed.map(_.text)),
      "capabilities" -> capabilities
    )
    server.instructions.foreach(i => result("instructions") = i)
    result("ttlMs") = 0
    result("cacheScope") = "private"
    result("_meta") = ujson.Obj(
      ServerInfoKey -> ujson.Obj("name" -> server.name, "version" -> server.version)
    )
    result
  }

  /** `initialize`'s result for `server`, asked for `asked`: that revision, else 2025-11-25. */
  private def initialized(asked: Option[Revision], server: ServerInfo): ujson.Obj = {
    val result = ujson.Obj(
      "protocolVersion" -> asked.filter(_ != Revision.Jul2026).getOrElse(Revision.Nov2025).text,
      "capabilities" -> capabilities,
      "serverInfo" -> ujson.Obj("name" -> server.name, "version" -> server.version)
    )
    server.instructions.foreach(i => result("instructions") = i)
    result
  }

  /** `result` answering request `id` in `revision`, marked complete in 2026-07-28. */
  private[wire] def answer(id: ujson.Value, revision: Revision, result: ujson.Obj): Response = {
    val marked =
      if (revision == Revision.Jul2026) ujson.Obj("resultType" -> "complete").obj ++= result.obj
      else result.obj
    Response.json(
      Status.Ok,
      ujson.Obj("jsonrpc" -> "2.0", "id" -> id, "result" -> ujson.Obj.from(marked))
    )
  }

  /** `refusal` as a JSON-RPC error, sent with `status`. */
  private[wire] def refused(refusal: Refusal, status: Status): Response = {
    val error = ujson.Obj("code" -> refusal.code, "message" -> refusal.text)
    refusal.data.foreach(d => error("data") = d)
    Response.json(status, ujson.Obj("jsonrpc" -> "2.0", "id" -> refusal.id, "error" -> error))
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

  private def error(e: ujson.Value): McpError = {
    val o = e.objOpt
    val code = o.flatMap(_.get("code")).flatMap(_.numOpt).filter(_.isWhole).map(_.toInt)
    (code, o.flatMap(_.get("message")).flatMap(_.strOpt)) match {
      case (Some(Code.UnsupportedVersion), _) =>
        McpError.Unsupported(
          o.flatMap(_.get("data"))
            .flatMap(_.objOpt)
            .flatMap(_.get("supported"))
            .flatMap(_.arrOpt)
            .fold(Vector.empty[String])(_.toVector.flatMap(_.strOpt))
        )
      case (Some(code), Some(text)) => McpError.Rpc(code, text)
      case _ => McpError.Unreadable("an error with no code and message")
    }
  }
}
