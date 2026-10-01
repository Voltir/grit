package grit.mcp.client

import java.net.http.{HttpClient, HttpRequest, HttpResponse}

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.mcp.wire.{Headers, McpError, McpTool, Rpc}

import utest.*

/** The exchanges an MCP server at revision 2026-07-28 must answer as grit's client reads them,
  * run against [[FakeMcpServer]] in the unit tier ([[McpServerFakeTests]]) and against
  * GitHub's by `LiveProbe`, so the fake is held to what a real server does.
  */
abstract class McpServerContract extends TestSuite {

  /** The server under test, declared with no allowlist. */
  protected def server: McpServer

  /** Its token. */
  protected def bearer: Bearer

  /** A tool it lists as read-only, by its own name, and arguments it answers without error. */
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

  /** `e`'s JSON-RPC error code, when it is a JSON-RPC error. */
  private def code(e: McpError): Option[Int] = e match {
    case McpError.Rpc(c, _) => Some(c)
    case _ => None
  }

  /** The HTTP status of a POST carrying `sent`, the token and `body`, and the JSON-RPC error
    * code its answer carries, when it carries one.
    */
  private def raw(sent: Vector[(String, String)], body: ujson.Value): (Int, Option[Int]) = {
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
    (response.statusCode, code(Rpc.failure(response.statusCode, response.body, None)))
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

    test("a token it does not know is refused: 401, with its challenge") {
      Bearer.of(Map("NOT_A_TOKEN" -> "grit-contract-not-a-token"), Variable("NOT_A_TOKEN")) match {
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
  fake.lists(Vector(FakeMcpServer.Weather, FakeMcpServer.snap("get_file_contents")))

  protected val server: McpServer =
    McpServer.of("fake", fake.endpoint, Variable("FAKE_MCP_TOKEN")) match {
      case Right(s) => s
      case Left(why) => throw new java.lang.AssertionError(why)
    }

  protected val bearer: Bearer =
    Bearer.of(Map("FAKE_MCP_TOKEN" -> fake.token), Variable("FAKE_MCP_TOKEN")) match {
      case Right(b) => b
      case Left(why) => throw new java.lang.AssertionError(why.message)
    }

  protected val read: (String, ujson.Obj) =
    ("get_weather", ujson.Obj("location" -> "New York"))

  override def utestAfterAll(): Unit = fake.stop()
}
