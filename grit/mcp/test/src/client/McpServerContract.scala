package grit.mcp.client

import java.net.http.{HttpClient, HttpRequest, HttpResponse}

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.mcp.scope.McpScope
import grit.mcp.wire.{Headers, McpError, McpTool, Rpc}

import utest.*

/** The exchanges an MCP server at revision 2026-07-28 must answer as grit's client reads them,
  * run against [[FakeMcpServer]] in the unit tier ([[McpServerFakeTests]]). `LiveProbe` runs it
  * against GitHub's at the live run, which holds the fake to a real server; until then it is
  * held only to the spec and to go-sdk's source.
  */
abstract class McpServerContract extends TestSuite {

  /** The server under test, declared with no allowlist. */
  protected def server: McpServer

  /** Its token. */
  protected def bearer: Bearer

  /** A token shaped as the server's tokens are that it does not know, so that it is refused
    * as unknown (401) rather than as malformed, which a server may answer otherwise (GitHub's
    * answers 400).
    */
  protected def unknownToken: String

  /** A tool it lists as read-only that mirrors an argument into a header (`x-mcp-header`), by
    * its own name, and arguments it answers without error, the mirrored one among them.
    */
  protected def read: (String, ujson.Obj)

  private def client(b: Bearer = bearer): McpClient^ = new McpClient(server, b, Clock.system())

  /** A read-only tool of `server`'s that it does not list. */
  private val unlisted: Option[McpTool] =
    McpTool
      .page(
        ujson.Obj(
          "tools" -> ujson.Arr(
            ujson.Obj(
              "name" -> "grit_contract_no_such_tool",
              "inputSchema" -> ujson.Obj("type" -> "object"),
              "annotations" -> ujson.Obj("readOnlyHint" -> true)
            )
          )
        ),
        "contract"
      )
      .toOption
      .flatMap(_.tools.headOption)

  /** The read tool as listed, when it mirrors an argument into a header. */
  private def mirroring: Option[McpTool] =
    client().tools().toOption.flatMap(_.tools.find(t => t.name == read._1 && t.params.nonEmpty))

  /** `e`'s JSON-RPC error code, when it is a JSON-RPC error. */
  private def code(e: McpError): Option[Int] = e match {
    case McpError.Rpc(c, _) => Some(c)
    case _ => None
  }

  /** The HTTP status of a POST carrying `sent`, the token and `body`, and its answer read as a
    * refusal ([[Rpc.failure]]).
    */
  private def answered(sent: Vector[(String, String)], body: ujson.Value): (Int, McpError) = {
    val response = HttpClient
      .newHttpClient()
      .send(
        sent
          .foldLeft(HttpRequest.newBuilder(server.endpoint)) { case (b, (k, v)) => b.header(k, v) }
          .header("Authorization", s"Bearer ${bearer.value}")
          .POST(HttpRequest.BodyPublishers.ofString(ujson.write(body)))
          .build(),
        HttpResponse.BodyHandlers.ofString()
      )
    (response.statusCode, Rpc.failure(response.statusCode, response.body, None))
  }

  /** [[answered]]'s status, and the JSON-RPC error code its answer carries, when it carries one. */
  private def raw(sent: Vector[(String, String)], body: ujson.Value): (Int, Option[Int]) = {
    val (status, e) = answered(sent, body)
    (status, code(e))
  }

  val tests = Tests {
    test("the list is read, and offers the read tool") {
      client().tools().map(_.tools.exists(_.name == read._1)) ==> Right(true)
    }

    test("the read tool, called, answers text that is not an error") {
      val c = client()
      c.tools()
        .flatMap(_.tools.find(_.name == read._1).toRight(McpError.Unreadable("not listed")))
        .flatMap(c.call(_, read._2))
        .map(a => (a.isError, a.text.nonEmpty)) ==> Right((false, true))
    }

    test("a call of the read tool without its Mcp-Param headers is rejected: 400 and -32020") {
      // streamable-http.mdx, Server Behavior for Custom Headers: a client that omits the header
      // while the value is in the body is non-conforming, and the server must reject it.
      mirroring.map { t =>
        val call = Rpc.Call.CallTool(t, read._2)
        raw(Headers.of(call).filterNot(_._1.startsWith("Mcp-Param-")), Rpc.request(1, call))
      } ==> Some((400, Some(-32020)))
    }

    test(
      "a call of the read tool whose Mcp-Param value is not its argument is rejected: 400 and -32020"
    ) {
      // streamable-http.mdx, Server Validation: a header that does not match the body.
      mirroring.map { t =>
        val call = Rpc.Call.CallTool(t, read._2)
        raw(
          Headers
            .of(call)
            .map((k, v) => if (k.startsWith("Mcp-Param-")) (k, s"$v-other") else (k, v)),
          Rpc.request(1, call)
        )
      } ==> Some((400, Some(-32020)))
    }

    test("a tool it does not list is rejected: 400 and -32602") {
      // SEP-2575: Invalid params is 400 on HTTP, as go-sdk, the SDK of GitHub's server, sends it.
      unlisted.map { t =>
        val call = Rpc.Call.CallTool(t, ujson.Obj())
        raw(Headers.of(call), Rpc.request(1, call))
      } ==> Some((400, Some(-32602)))
    }

    test("a request without Mcp-Method is rejected: 400 and -32020") {
      // streamable-http.mdx, Server Validation: a required standard header missing.
      val call = Rpc.Call.ListTools(None)
      raw(Headers.of(call).filter(_._1 != "Mcp-Method"), Rpc.request(1, call)) ==>
        (400, Some(-32020))
    }

    test("a version it does not speak is rejected: 400, -32022, naming 2026-07-28 as one it does") {
      // basic/versioning.mdx; a version later than 2026-07-28, which go-sdk refuses in JSON-RPC.
      val call = Rpc.Call.ListTools(None)
      val later = "2099-12-31"
      val body = Rpc.request(1, call)
      body("params")("_meta")("io.modelcontextprotocol/protocolVersion") = later
      val (status, e) = answered(
        Headers.of(call).map((k, v) => if (k == "MCP-Protocol-Version") (k, later) else (k, v)),
        body
      )
      (
        status,
        e match {
          case McpError.Unsupported(supported) => Some(supported.contains(Rpc.Version))
          case _ => None
        }
      ) ==> (400, Some(true))
    }

    test("a call whose Mcp-Name is not the body's tool is rejected: 400 and -32020") {
      // streamable-http.mdx, Server Validation: a header that does not match the body.
      unlisted.map { t =>
        val call = Rpc.Call.CallTool(t, ujson.Obj())
        raw(
          Headers
            .of(call)
            .map((k, v) => if (k == "Mcp-Name") (k, "grit_contract_other") else (k, v)),
          Rpc.request(1, call)
        )
      } ==> Some((400, Some(-32020)))
    }

    test("a request whose _meta lacks clientCapabilities is rejected: 400 and -32602") {
      // basic/index.mdx, Per-request protocol fields: a required field missing.
      val call = Rpc.Call.ListTools(None)
      val body = Rpc.request(1, call)
      val _ = body("params")("_meta").obj.remove("io.modelcontextprotocol/clientCapabilities")
      raw(Headers.of(call), body) ==> (400, Some(-32602))
    }

    test("a request whose Accept lacks text/event-stream is rejected: 400, with no MCP error") {
      // streamable-http.mdx, Sending Messages: a client lists both types it reads. The status is
      // go-sdk's (streamable.go, serveStateless), as is the next case's.
      val call = Rpc.Call.ListTools(None)
      raw(
        Headers.of(call).map((k, v) => if (k == "Accept") (k, "application/json") else (k, v)),
        Rpc.request(1, call)
      ) ==> (400, None)
    }

    test("a request whose Content-Type is not application/json is rejected: 415") {
      val call = Rpc.Call.ListTools(None)
      raw(
        Headers.of(call).map((k, v) => if (k == "Content-Type") (k, "text/plain") else (k, v)),
        Rpc.request(1, call)
      ) ==> (415, None)
    }

    test("a token it does not know is refused: 401, with its challenge") {
      Bearer.of(Map("UNKNOWN_TOKEN" -> unknownToken), Variable("UNKNOWN_TOKEN")) match {
        case Right(wrong) =>
          // The challenge's scheme: the rest is the server's to word.
          client(wrong).tools().map(_.tools.size).left.map {
            case McpError.Unauthorized(challenge) => challenge.map(_.takeWhile(_ != ' '))
            case other => Some(other.message)
          } ==> Left(Some("Bearer"))
        case Left(why) => throw new java.lang.AssertionError(why.message)
      }
    }
  }
}

/** The contract, kept by the fake. */
object McpServerFakeTests extends McpServerContract {

  private val fake = FakeMcpServer.start()
  fake.lists(Vector(FakeMcpServer.Weather, FakeMcpServer.github("get_file_contents")))

  protected val server: McpServer =
    McpServer.of("fake", fake.endpoint, Variable("FAKE_MCP_TOKEN"), McpScope.Open) match {
      case Right(s) => s
      case Left(why) => throw new java.lang.AssertionError(why)
    }

  protected val bearer: Bearer =
    Bearer.of(Map("FAKE_MCP_TOKEN" -> fake.token), Variable("FAKE_MCP_TOKEN")) match {
      case Right(b) => b
      case Left(why) => throw new java.lang.AssertionError(why.message)
    }

  protected val unknownToken: String = "fake-unknown-token"

  protected val read: (String, ujson.Obj) = (
    "get_file_contents",
    ujson.Obj("owner" -> "the-actual-best", "repo" -> "actualbest", "path" -> "README.md")
  )

  override def utestAfterAll(): Unit = fake.stop()
}
