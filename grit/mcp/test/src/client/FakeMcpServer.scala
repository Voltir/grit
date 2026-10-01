package grit.mcp.client

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64
import java.util.concurrent.{CountDownLatch, Executors}

import scala.jdk.CollectionConverters.*

import com.sun.net.httpserver.{HttpExchange, HttpServer}

/** An MCP server at revision 2026-07-28 in this JVM, over Streamable HTTP on a loopback port,
  * held to [[McpServerContract]] as GitHub's is. It checks every request as the spec's server
  * rules say (`streamable-http.mdx` §Server Validation, `basic/index.mdx` §`_meta`) and
  * rejects one that breaks them as a real server must: `401` without its token, `400` with
  * `-32020` for a missing or mismatched `MCP-Protocol-Version`, `Mcp-Method` or `Mcp-Name`,
  * `400` with `-32602` for `_meta` without its required fields, and `400` with `-32022` for a
  * version it does not [[speaks]]. It answers `tools/list` from [[lists]], a page at a time,
  * and `tools/call` from [[answers]] (an unlisted tool, like an unknown cursor, is `400` with `-32602`), in JSON or, after
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
    message match {
      case None => error(400, -32700, "Parse error")
      case Some(_)
          if !(accept.contains("application/json") && accept.contains("text/event-stream")) =>
        Reply(406, Vector.empty, "")
      case Some(m) =>
        val method = m.get("method").flatMap(_.strOpt)
        val params = m.get("params").flatMap(_.objOpt)
        val meta = params.flatMap(_.get("_meta")).flatMap(_.objOpt)
        val version =
          meta.flatMap(_.get("io.modelcontextprotocol/protocolVersion")).flatMap(_.strOpt)
        val capabilities =
          meta.flatMap(_.get("io.modelcontextprotocol/clientCapabilities")).flatMap(_.objOpt)
        val name = params.flatMap(_.get("name")).flatMap(_.strOpt)
        (version, capabilities) match {
          case (None, _) | (_, None) =>
            error(400, -32602, "Invalid params: _meta lacks a required field")
          case (Some(v), _) if !headers.get("mcp-protocol-version").contains(v) =>
            error(400, -32020, s"Header mismatch: MCP-Protocol-Version does not match _meta ($v)")
          case (Some(v), _) if !script.versions.contains(v) =>
            error(
              400,
              -32022,
              s"Unsupported protocol version: $v",
              Some(ujson.Obj("supported" -> ujson.Arr.from(script.versions), "requested" -> v))
            )
          case _ if method.isEmpty || headers.get("mcp-method") != method =>
            error(400, -32020, "Header mismatch: Mcp-Method does not match the body's method")
          case _ if method.contains("tools/call") && headers.get("mcp-name").map(decoded) != name =>
            error(400, -32020, "Header mismatch: Mcp-Name does not match the body's params.name")
          case _ =>
            method match {
              case Some("tools/list") =>
                val cursor = params.flatMap(_.get("cursor")).flatMap(_.strOpt)
                val pages = script.tools.grouped(script.perPage).toVector
                val at = cursor.fold(Some(0))(c => pages.indices.find(i => i > 0 && Cursor(i) == c))
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
                val listed = script.tools.flatMap(_.obj.get("name")).flatMap(_.strOpt)
                name.filter(listed.contains) match {
                  case None => error(400, -32602, s"Unknown tool: ${name.getOrElse("")}")
                  case Some(n) =>
                    val arguments = params.flatMap(_.get("arguments")).getOrElse(ujson.Obj())
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
    if (bytes.nonEmpty)
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

  /** GitHub's own `tools/list` entry for `name` (see grit/mcp/README.md): `get_file_contents`,
    * `get_me` (read-only) or `issue_write` (not).
    */
  def snap(name: String): ujson.Obj =
    ujson.Obj.from(
      ujson
        .read(
          scala.io.Source
            .fromInputStream(classOf[FakeMcpServer].getResourceAsStream(s"/toolsnaps/$name.snap"))
            .mkString
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
