package grit.mcp.client

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util.concurrent.{CountDownLatch, Executors}

import scala.jdk.CollectionConverters.*

import com.sun.net.httpserver.{HttpExchange, HttpServer}

/** An MCP server at revision 2026-07-28 in this JVM, over Streamable HTTP on a loopback port,
  * held to [[McpServerContract]]. It checks every request as the spec's server
  * rules say (`streamable-http.mdx` §Server Validation, `basic/index.mdx` §`_meta`), in the
  * order go-sdk's stateless server does, and rejects one that breaks them: `401` without its
  * token; `415` for a `Content-Type` other than `application/json`; `400` in plain text for an
  * `Accept` lacking either type it answers in, or a body that is not JSON; `400` with `-32020`
  * for a missing `MCP-Protocol-Version`, or one, an `Mcp-Method` or an `Mcp-Name` that does
  * not match the body, or for a call an `Mcp-Param-*` header missing, unexpected or not
  * matching its argument, as the listed tool's `x-mcp-header` annotations say; `400` with `-32602` for `_meta` without its protocol version or client
  * capabilities; and `400` with `-32022` for a version it does not [[speaks]]. It answers
  * `tools/list` from [[lists]], a page at a time, and `tools/call` from [[answers]] (an
  * unlisted tool, like an unknown cursor, is `400` with `-32602`), in JSON or, after
  * [[streams]], an event stream with a comment and a notification before the response. Every
  * request it was sent is kept, in [[received]].
  */
final class FakeMcpServer private (val token: String) {
  import FakeMcpServer.*

  // Test scaffolding: the script below is set by the test's thread and read by the server's,
  // each under `lock`, and no capability is reached through any of it.
  private val lock = new Object

  @caps.unsafe.untrackedCaptures
  private var tools = Vector.empty[ujson.Obj]

  @caps.unsafe.untrackedCaptures
  private var perPage = Int.MaxValue

  @caps.unsafe.untrackedCaptures
  private var ttls = Vector[Option[Long]](Some(0L))

  @caps.unsafe.untrackedCaptures
  private var results = Map.empty[String, ujson.Obj]

  @caps.unsafe.untrackedCaptures
  private var sse = false

  @caps.unsafe.untrackedCaptures
  private var versions = Vector("2026-07-28")

  @caps.unsafe.untrackedCaptures
  private var refusal: Option[Refusal] = None

  @caps.unsafe.untrackedCaptures
  private var stalled = false

  @caps.unsafe.untrackedCaptures
  private var log = Vector.empty[Received]

  // Released when the server stops, so a stalled exchange's thread ends.
  private val stopping = new CountDownLatch(1)

  private val http: HttpServer = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
  http.setExecutor(Executors.newCachedThreadPool { (r: Runnable) =>
    val t = new Thread(r, "fake-mcp")
    t.setDaemon(true)
    t
  })
  http.createContext("/mcp", (exchange: HttpExchange) => handle(exchange))
  http.start()

  /** The URL it answers at. */
  def endpoint: String = s"http://127.0.0.1:${http.getAddress.getPort}/mcp"

  /** Lists `listed` from now on, `pageSize` to a page (each page after the first named by an
    * opaque cursor), page `i`'s `ttlMs` the `i`th of `ttlMs` or its last (`None` omits it).
    */
  def lists(
      listed: Vector[ujson.Obj],
      pageSize: Int = Int.MaxValue,
      ttlMs: Vector[Option[Long]] = Vector(Some(0L))
  ): Unit = lock.synchronized {
    tools = listed
    perPage = pageSize.max(1)
    ttls = if (ttlMs.isEmpty) Vector(Some(0L)) else ttlMs
  }

  /** Answers a `tools/call` of `name` with `result`, a `CallToolResult`, from now on. A listed
    * tool with no answer set answers one text block, `called {name} with {arguments}`.
    */
  def answers(name: String, result: ujson.Obj): Unit =
    lock.synchronized { results = results.updated(name, result) }

  /** Answers in an event stream when `on`, else in JSON (the default). */
  def streams(on: Boolean): Unit = lock.synchronized { sse = on }

  /** The protocol versions it takes, from now on; only `2026-07-28` by default. */
  def speaks(taken: Vector[String]): Unit = lock.synchronized { versions = taken }

  /** Answers every request with `status`, `headers` and `body` from now on, before reading it;
    * `None` answers as the script says again.
    */
  def refuses(answer: Option[Refusal]): Unit = lock.synchronized { refusal = answer }

  /** Starts every response as an event stream with a comment, then sends nothing more until
    * the server stops, from now on when `on`.
    */
  def stalls(on: Boolean): Unit = lock.synchronized { stalled = on }

  /** Every request it was sent, oldest first, those it rejected included. */
  def received: Vector[Received] = lock.synchronized(log)

  /** How many requests it was sent with method `method`. */
  def count(method: String): Int = received.count(_.method.contains(method))

  /** Stops answering; a stalled exchange ends. */
  def stop(): Unit = {
    stopping.countDown()
    http.stop(0)
  }

  private def handle(exchange: HttpExchange): Unit =
    try {
      val body = new String(exchange.getRequestBody.readAllBytes(), UTF_8)
      val headers = exchange.getRequestHeaders.asScala.toMap.map((k, vs) =>
        k.toLowerCase -> vs.asScala.mkString(",")
      )
      val message = scala.util.Try(ujson.read(body)).toOption.flatMap(_.objOpt)
      val method = message.flatMap(_.get("method")).flatMap(_.strOpt)
      val script = lock.synchronized {
        log = log :+ Received(method, headers, body)
        Script(tools, perPage, ttls, results, sse, versions, refusal, stalled)
      }
      val reply =
        if (exchange.getRequestMethod != "POST") Reply(405, Vector.empty, "")
        else if (!headers.get("authorization").contains(s"Bearer $token"))
          Reply(401, Vector("WWW-Authenticate" -> Challenge), "")
        else
          script.refusal match {
            case Some(r) => Reply(r.status, r.headers, r.body)
            case None => answer(headers, message, script)
          }
      if (script.stalled && reply.status == 200) stall(exchange)
      else send(exchange, reply, script.sse)
    } finally exchange.close()

  /** The reply to a request with `headers` and `message`, as the spec's server rules say. */
  private def answer(
      headers: Map[String, String],
      message: Option[collection.Map[String, ujson.Value]],
      script: Script
  ): Reply = {
    val id = message.flatMap(_.get("id")).getOrElse(ujson.Null)
    def error(status: Int, code: Int, said: String, data: Option[ujson.Value] = None): Reply = {
      val e = ujson.Obj("code" -> code, "message" -> said)
      data.foreach(d => e("data") = d)
      Reply(
        status,
        Vector.empty,
        ujson.write(ujson.Obj("jsonrpc" -> "2.0", "id" -> id, "error" -> e))
      )
    }
    val accept = headers.getOrElse("accept", "")
    val contentType = headers.getOrElse("content-type", "").takeWhile(_ != ';').trim.toLowerCase
    // go-sdk's order (streamable.go: serveStateless, servePOST; server.go: handle).
    if (contentType != "application/json")
      plain(415, "Content-Type must be 'application/json'")
    else if (!(accept.contains("application/json") && accept.contains("text/event-stream")))
      plain(400, "Accept must contain both 'application/json' and 'text/event-stream'")
    else
      message match {
        case None => plain(400, "malformed payload")
        case Some(m) =>
          val method = m.get("method").flatMap(_.strOpt)
          val params = m.get("params").flatMap(_.objOpt)
          val meta = params.flatMap(_.get("_meta")).flatMap(_.objOpt)
          val version =
            meta.flatMap(_.get("io.modelcontextprotocol/protocolVersion")).flatMap(_.strOpt)
          val capabilities =
            meta.flatMap(_.get("io.modelcontextprotocol/clientCapabilities")).flatMap(_.objOpt)
          val name = params.flatMap(_.get("name")).flatMap(_.strOpt)
          val header = headers.get("mcp-protocol-version")
          (header, version) match {
            case (None, _) =>
              error(400, -32020, "MCP-Protocol-Version header is required")
            case (_, None) =>
              error(400, -32602, "Invalid params: _meta lacks its protocolVersion")
            case (Some(h), Some(v)) if h != v =>
              error(
                400,
                -32020,
                s"Header mismatch: MCP-Protocol-Version $h does not match _meta's $v"
              )
            case _ if method.isEmpty || headers.get("mcp-method") != method =>
              error(400, -32020, "Header mismatch: Mcp-Method does not match the body's method")
            case _
                if method.contains("tools/call") && headers.get("mcp-name").map(decoded) != name =>
              error(400, -32020, "Header mismatch: Mcp-Name does not match the body's params.name")
            case _ if capabilities.isEmpty =>
              error(400, -32602, "Invalid params: _meta lacks its clientCapabilities")
            case (_, Some(v)) if !script.versions.contains(v) =>
              error(
                400,
                -32022,
                s"Unsupported protocol version: $v",
                Some(ujson.Obj("supported" -> ujson.Arr.from(script.versions), "requested" -> v))
              )
            case _ =>
              method match {
                case Some("tools/list") =>
                  val cursor = params.flatMap(_.get("cursor")).flatMap(_.strOpt)
                  val pages = script.tools.grouped(script.perPage).toVector
                  val at =
                    cursor.fold(Some(0))(c => pages.indices.find(i => i > 0 && Cursor(i) == c))
                  at match {
                    case None => error(400, -32602, "Invalid params: unknown cursor")
                    case Some(i) =>
                      val result = ujson.Obj(
                        "resultType" -> "complete",
                        "tools" -> ujson.Arr.from(pages.lift(i).getOrElse(Vector.empty))
                      )
                      script.ttls.lift(i).orElse(script.ttls.lastOption).flatten.foreach { t =>
                        result("ttlMs") = ujson.Num(t.toDouble)
                      }
                      if (i + 1 < pages.size) result("nextCursor") = Cursor(i + 1)
                      ok(id, result)
                  }
                case Some("tools/call") =>
                  val listed = name.flatMap(n =>
                    script.tools.find(_.obj.get("name").contains(ujson.Str(n))).map(t => (n, t))
                  )
                  val arguments = params.flatMap(_.get("arguments")).getOrElse(ujson.Obj())
                  listed.map((n, t) => (n, mismatch(t, arguments, headers))) match {
                    case None => error(400, -32602, s"Unknown tool: ${name.getOrElse("")}")
                    case Some((_, Some(why))) => error(400, -32020, why)
                    case Some((n, None)) =>
                      ok(
                        id,
                        script.results.getOrElse(
                          n,
                          ujson.Obj(
                            "content" -> ujson.Arr(
                              ujson.Obj(
                                "type" -> "text",
                                "text" -> s"called $n with ${ujson.write(arguments)}"
                              )
                            )
                          )
                        )
                      )
                  }
                case _ => error(404, -32601, s"Method not found: ${method.getOrElse("")}")
              }
          }
      }
  }

  /** Why `headers` do not mirror `arguments` as `tool`'s `x-mcp-header` annotations say, as
    * go-sdk's server checks them (streamable_headers.go, validateParamHeaders): a header for an
    * argument absent or null, none for one present, or one whose decoded value is not the
    * argument's (an integer compared as a number).
    */
  private def mismatch(
      tool: ujson.Obj,
      arguments: ujson.Value,
      headers: Map[String, String]
  ): Option[String] = {
    def annotated(schema: ujson.Value, path: Vector[String]): Vector[(Vector[String], String)] =
      schema.objOpt.flatMap(_.get("properties")).flatMap(_.objOpt).toVector.flatMap {
        _.toVector.flatMap { (key, property) =>
          property.objOpt
            .flatMap(_.get("x-mcp-header"))
            .flatMap(_.strOpt)
            .toVector
            .map(h => (path :+ key, h)) ++ annotated(property, path :+ key)
        }
      }
    def at(path: Vector[String]): Option[ujson.Value] =
      path.foldLeft(Option(arguments))((v, k) => v.flatMap(_.objOpt).flatMap(_.get(k)))
    annotated(tool.obj.getOrElse("inputSchema", ujson.Obj()), Vector.empty).flatMap {
      (path: Vector[String], header: String) =>
        val name = s"Mcp-Param-$header"
        val sent = headers.get(name.toLowerCase)
        val where = path.mkString(".")
        (at(path).filter(_ != ujson.Null), sent) match {
          case (None, None) => None
          case (None, Some(_)) =>
            Some(
              s"header mismatch: unexpected $name header for absent or null parameter \"$where\""
            )
          case (Some(_), None) =>
            Some(s"header mismatch: missing $name header for parameter \"$where\"")
          case (Some(value), Some(h)) =>
            val decodedHeader = decoded(h)
            val same = value match {
              case ujson.Str(v) => decodedHeader == v
              case ujson.Bool(v) => decodedHeader == v.toString
              case ujson.Num(v) => decodedHeader.toDoubleOption.contains(v)
              case _ => false
            }
            Option.when(!same)(
              s"header mismatch: $name header value '$h' does not match body value"
            )
        }
    }.headOption
  }

  /** A refusal in plain text, as go-sdk's `http.Error` sends one. */
  private def plain(status: Int, said: String): Reply =
    Reply(status, Vector("Content-Type" -> "text/plain; charset=utf-8"), said)

  private def ok(id: ujson.Value, result: ujson.Obj): Reply =
    Reply(
      200,
      Vector.empty,
      ujson.write(ujson.Obj("jsonrpc" -> "2.0", "id" -> id, "result" -> result))
    )

  private def send(exchange: HttpExchange, reply: Reply, sse: Boolean): Unit = {
    val streamed = sse && reply.status == 200
    val bytes =
      (if (streamed)
         ": a comment, passed over\n\n" +
           "data: " + ujson.write(
             ujson.Obj(
               "jsonrpc" -> "2.0",
               "method" -> "notifications/message",
               "params" -> ujson.Obj("level" -> "info", "data" -> "working")
             )
           ) + "\n\n" +
           "data: " + reply.body + "\n\n"
       else reply.body).getBytes(UTF_8)
    val h = exchange.getResponseHeaders
    reply.headers.foreach((k, v) => h.add(k, v))
    if (bytes.nonEmpty && !h.containsKey("Content-Type"))
      h.set("Content-Type", if (streamed) "text/event-stream" else "application/json")
    exchange.sendResponseHeaders(reply.status, if (bytes.isEmpty) -1 else bytes.length.toLong)
    if (bytes.nonEmpty) exchange.getResponseBody.write(bytes)
  }

  private def stall(exchange: HttpExchange): Unit = {
    exchange.getResponseHeaders.set("Content-Type", "text/event-stream")
    exchange.sendResponseHeaders(200, 0)
    val out = exchange.getResponseBody
    out.write(": waiting\n\n".getBytes(UTF_8))
    out.flush()
    stopping.await()
  }
}

object FakeMcpServer {

  /** A fake that takes `token` and lists nothing yet. */
  def start(token: String = "fake-token"): FakeMcpServer = new FakeMcpServer(token)

  /** What it answers every request with, after [[FakeMcpServer.refuses]]. */
  final case class Refusal(status: Int, headers: Vector[(String, String)], body: String)

  /** A request it was sent: its JSON-RPC method, when it had one; its headers, each name in
    * lowercase, repeated values joined by commas; its body.
    */
  final case class Received(method: Option[String], headers: Map[String, String], body: String)

  /** The challenge its 401 carries (`basic/authorization/index.mdx` §Error Handling). */
  val Challenge: String =
    "Bearer error=\"invalid_token\", resource_metadata=\"https://example.com/.well-known/oauth-protected-resource\""

  /** The spec's own listed tool (server/tools.mdx §Listing Tools), marked read-only. */
  val Weather: ujson.Obj = ujson.Obj(
    "name" -> "get_weather",
    "title" -> "Weather Information Provider",
    "description" -> "Get current weather information for a location",
    "inputSchema" -> ujson.Obj(
      "type" -> "object",
      "properties" -> ujson.Obj(
        "location" -> ujson.Obj("type" -> "string", "description" -> "City name or zip code")
      ),
      "required" -> ujson.Arr("location")
    ),
    "annotations" -> ujson.Obj("readOnlyHint" -> true)
  )

  /** GitHub's own write tool `issue_write`'s `tools/list` entry, from its source (see
    * grit/mcp/README.md); the hosted read-only server does not list it.
    */
  def issueWrite: ujson.Obj = resource("/toolsnaps/issue_write.snap")

  /** The `tools/list` entry for `name` from GitHub's hosted read-only server's list, as it
    * answered (see grit/mcp/README.md): `get_file_contents` and `get_me` among them.
    */
  def github(name: String): ujson.Obj =
    resource("/github/tools-list.json")("tools").arr
      .find(_.obj.get("name").contains(ujson.Str(name)))
      .fold(throw new java.lang.AssertionError(s"$name is not in GitHub's list"))(t =>
        ujson.Obj.from(t.obj)
      )

  private def resource(path: String): ujson.Obj =
    ujson.Obj.from(
      ujson
        .read(
          scala.io.Source.fromInputStream(classOf[FakeMcpServer].getResourceAsStream(path)).mkString
        )
        .obj
    )

  private def Cursor(page: Int): String = s"page-$page"

  private val Open = "=?base64?"
  private val Close = "?="

  /** A header value as the spec's Value Encoding reads it. */
  private def decoded(value: String): String =
    if (
      value.startsWith(Open) && value.endsWith(Close) && value.length >= Open.length + Close.length
    )
      scala.util
        .Try(
          new String(
            Base64.getDecoder.decode(value.drop(Open.length).dropRight(Close.length)),
            UTF_8
          )
        )
        .getOrElse(value)
    else value

  private final case class Reply(status: Int, headers: Vector[(String, String)], body: String)

  private final case class Script(
      tools: Vector[ujson.Obj],
      perPage: Int,
      ttls: Vector[Option[Long]],
      results: Map[String, ujson.Obj],
      sse: Boolean,
      versions: Vector[String],
      refusal: Option[Refusal],
      stalled: Boolean
  )
}
