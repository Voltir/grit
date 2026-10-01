package grit.mcp.client

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import grit.core.clock.Clock
import grit.core.edge.Variable
import grit.core.tool.ToolName
import grit.mcp.wire.{McpError, Skipped}

import utest.*

/** [[McpClient]] against [[FakeMcpServer]]: the list read, kept while fresh, and calls made. */
object McpClientTests extends TestSuite {

  /** A clock that reads `at` milliseconds, set by the test; each reading adds `step`. */
  final class Hands(start: Long = 0L, step: Long = 0L) extends Clock {
    // Test scaffolding: set by the test's thread, read by the client's, and no capability is
    // reached through it.
    @volatile @caps.unsafe.untrackedCaptures
    var at: Long = start

    def now(): Instant = Instant.ofEpochMilli(at)
    def millis(): Long = { val m = at; at += step; m }
    def sleep(duration: FiniteDuration): Unit = at += duration.toMillis
  }

  private val Token = Variable("FAKE_MCP_TOKEN")

  /** A client of `fake`, as the server `name` allowing `allow`, its token `token`. */
  private def client(
      fake: FakeMcpServer,
      clock: Clock = new Hands(),
      allow: Set[String] = Set.empty,
      token: String = "fake-token"
  ): McpClient^{clock} = {
    val server = McpServer.of("fake", fake.endpoint, Token, allow) match {
      case Right(s) => s
      case Left(why) => throw new java.lang.AssertionError(why)
    }
    val bearer = Bearer.of(Map("FAKE_MCP_TOKEN" -> token), Token) match {
      case Right(b) => b
      case Left(why) => throw new java.lang.AssertionError(why.message)
    }
    new McpClient(server, bearer, clock)
  }

  /** A read-only tool `name` with an empty object schema. */
  private def tool(name: String): ujson.Obj = ujson.Obj(
    "name" -> name,
    "description" -> s"$name.",
    "inputSchema" -> ujson.Obj("type" -> "object"),
    "annotations" -> ujson.Obj("readOnlyHint" -> true)
  )

  /** Runs `body` with a fresh fake, stopped after. */
  private def withFake[A](body: FakeMcpServer => A): A = {
    val fake = FakeMcpServer.start()
    try body(fake)
    finally fake.stop()
  }

  private def offered(listed: Either[McpError, Listed]): Either[McpError, Vector[String]] =
    listed.map(_.tools.map(t => ToolName.value(t.offered)))

  val tests = Tests {
    test("every page is read, in order, its tools offered under the server's prefix") {
      withFake { fake =>
        fake.lists(Vector("a", "b", "c", "d", "e").map(tool), pageSize = 2)
        offered(client(fake).tools()) ==>
          Right(Vector("fake_a", "fake_b", "fake_c", "fake_d", "fake_e"))
        fake.count("tools/list") ==> 3
      }
    }

    test("a list sent as an event stream is read as one sent as JSON") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.Weather, FakeMcpServer.github("get_me")), pageSize = 1)
        fake.streams(true)
        offered(client(fake).tools()) ==> Right(Vector("fake_get_weather", "fake_get_me"))
      }
    }

    test("each request carries the version, method and token, and _meta, the server checks") {
      withFake { fake =>
        fake.lists(Vector(tool("a")))
        client(fake).tools()
        fake.received.map(r =>
          (
            r.headers.get("mcp-protocol-version"),
            r.headers.get("mcp-method"),
            r.headers.get("authorization"),
            r.headers.get("accept"),
            ujson.read(r.body)("params")("_meta")("io.modelcontextprotocol/protocolVersion").str
          )
        ) ==> Vector(
          (
            Some("2026-07-28"),
            Some("tools/list"),
            Some("Bearer fake-token"),
            Some("application/json, text/event-stream"),
            "2026-07-28"
          )
        )
      }
    }

    test("only the tools the allowlist names are offered; the others are skipped as not allowed") {
      withFake { fake =>
        fake.lists(
          Vector(tool("a"), FakeMcpServer.issueWrite, tool("b"), tool("c"))
        )
        client(fake, allow = Set("a", "c", "issue_write"))
          .tools()
          .map(l => (l.tools.map(t => ToolName.value(t.offered)), l.skipped)) ==> Right(
          (
            Vector("fake_a", "fake_c"),
            Vector(Skipped.NotReadOnly("issue_write"), Skipped.NotAllowed("b"))
          )
        )
      }
    }

    test("a tool listed twice is offered once") {
      withFake { fake =>
        fake.lists(Vector(tool("a"), tool("b"), tool("a")), pageSize = 2)
        offered(client(fake).tools()) ==> Right(Vector("fake_a", "fake_b"))
      }
    }

    test("a list is kept while fresh, and listed again once ttlMs has passed since it came") {
      withFake { fake =>
        fake.lists(Vector(tool("a")), ttlMs = Vector(Some(1000L)))
        val clock = new Hands(start = 5000L)
        val c = client(fake, clock)
        c.tools()
        clock.at = 5999L
        offered(c.tools()) ==> Right(Vector("fake_a"))
        fake.count("tools/list") ==> 1
        fake.lists(Vector(tool("b")), ttlMs = Vector(Some(1000L)))
        clock.at = 6000L
        offered(c.tools()) ==> Right(Vector("fake_b"))
        fake.count("tools/list") ==> 2
      }
    }

    test("a list with no ttlMs, or 0, is stale at once") {
      withFake { fake =>
        val c = client(fake)
        fake.lists(Vector(tool("a")), ttlMs = Vector(None))
        c.tools()
        c.tools()
        fake.lists(Vector(tool("a")), ttlMs = Vector(Some(0L)))
        c.tools()
        fake.count("tools/list") ==> 3
      }
    }

    test("a list of pages is fresh while its least fresh page is") {
      withFake { fake =>
        fake.lists(
          Vector(tool("a"), tool("b"), tool("c")),
          pageSize = 1,
          ttlMs = Vector(Some(5000L), Some(1000L), Some(9000L))
        )
        val clock = new Hands()
        val c = client(fake, clock)
        c.tools()
        clock.at = 999L
        c.tools()
        fake.count("tools/list") ==> 3
        clock.at = 1000L
        c.tools()
        fake.count("tools/list") ==> 6
      }
    }

    test("a list made stale is listed again while still fresh") {
      withFake { fake =>
        fake.lists(Vector(tool("a")), ttlMs = Vector(Some(60000L)))
        val c = client(fake)
        c.tools()
        c.stale()
        c.tools()
        fake.count("tools/list") ==> 2
      }
    }

    test("a re-list that fails serves the kept list; with none kept, its error") {
      withFake { fake =>
        fake.lists(Vector(tool("a")), ttlMs = Vector(Some(1000L)))
        val clock = new Hands()
        val c = client(fake, clock)
        c.tools()
        fake.refuses(Some(FakeMcpServer.Refusal(503, Vector.empty, "")))
        clock.at = 2000L
        offered(c.tools()) ==> Right(Vector("fake_a"))
        offered(client(fake).tools()) ==> Left(McpError.Status(503))
        fake.count("tools/list") ==> 3
      }
    }

    test("a list longer than MaxPages pages is unreadable, not followed for ever") {
      withFake { fake =>
        fake.lists(Vector.tabulate(McpClient.MaxPages + 1)(i => tool(s"t$i")), pageSize = 1)
        client(fake).tools().map(_.tools.size) ==>
          Left(McpError.Unreadable(s"tools/list ran past ${McpClient.MaxPages} pages"))
        fake.lists(Vector.tabulate(McpClient.MaxPages)(i => tool(s"t$i")), pageSize = 1)
        client(fake).tools().map(_.tools.size) ==> Right(McpClient.MaxPages)
      }
    }

    test("a call's answer is the tool's result as the model reads it, isError kept") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.Weather))
        // server/tools.mdx, Calling Tools, and Error Handling: the spec's own results.
        fake.answers(
          "get_weather",
          ujson.Obj(
            "content" -> ujson.Arr(
              ujson.Obj(
                "type" -> "text",
                "text" -> "Current weather in New York:\nTemperature: 72°F\nConditions: Partly cloudy"
              )
            ),
            "isError" -> false
          )
        )
        val c = client(fake)
        def weather(): Either[McpError, (String, Boolean)] =
          c.tools()
            .flatMap(_.tools.find(_.name == "get_weather").toRight(McpError.Unreadable("unlisted")))
            .flatMap(c.call(_, ujson.Obj("location" -> "New York")))
            .map(a => (a.text, a.isError))
        weather() ==> Right(
          ("Current weather in New York:\nTemperature: 72°F\nConditions: Partly cloudy", false)
        )
        fake.answers(
          "get_weather",
          ujson.Obj(
            "content" -> ujson.Arr(
              ujson.Obj(
                "type" -> "text",
                "text" -> "Invalid departure date: must be in the future. Current date is 08/08/2025."
              )
            ),
            "isError" -> true
          )
        )
        weather() ==> Right(
          ("Invalid departure date: must be in the future. Current date is 08/08/2025.", true)
        )
        fake.received.lastOption.map(r =>
          (r.method, r.headers.get("mcp-name"), ujson.read(r.body)("params")("arguments"))
        ) ==> Some((Some("tools/call"), Some("get_weather"), ujson.Obj("location" -> "New York")))
      }
    }

    test("a call answered input_required is refused: grit gives no input") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.Weather))
        // server/tools.mdx, Input Required Tool Results.
        fake.answers(
          "get_weather",
          ujson.Obj(
            "resultType" -> "input_required",
            "inputRequests" -> ujson.Obj(
              "github_login" -> ujson.Obj("method" -> "elicitation/create", "params" -> ujson.Obj())
            ),
            "requestState" -> "eyJsb2NhdGlvbiI6Ik5ldyBZb3JrIn0..."
          )
        )
        val c = client(fake)
        c.tools()
          .flatMap(_.tools.find(_.name == "get_weather").toRight(McpError.Unreadable("unlisted")))
          .flatMap(c.call(_, ujson.Obj("location" -> "New York")))
          .map(_.text) ==> Left(McpError.InputRequired)
      }
    }

    test("a tool the server no longer lists is a -32602 error at 400, and makes the list stale") {
      withFake { fake =>
        fake.lists(Vector(tool("a"), tool("gone")), ttlMs = Vector(Some(60000L)))
        val c = client(fake)
        val gone = c.tools().toOption.flatMap(_.tools.find(_.name == "gone"))
        fake.lists(Vector(tool("a")), ttlMs = Vector(Some(60000L)))
        gone.map(c.call(_, ujson.Obj())) ==>
          Some(Left(McpError.Rpc(-32602, "Unknown tool: gone")))
        offered(c.tools()) ==> Right(Vector("fake_a"))
        fake.count("tools/list") ==> 2
      }
    }

    test("each refusal is the McpError its status and body mean, its challenge kept") {
      withFake { fake =>
        fake.lists(Vector(tool("a")))
        def listed(c: McpClient^): Either[McpError, Int] = c.tools().map(_.tools.size)
        val scope = "Bearer error=\"insufficient_scope\", scope=\"repo\""
        listed(client(fake, token = "not-the-token")) ==>
          Left(McpError.Unauthorized(Some(FakeMcpServer.Challenge)))
        fake.refuses(Some(FakeMcpServer.Refusal(403, Vector("WWW-Authenticate" -> scope), "")))
        listed(client(fake)) ==> Left(McpError.Forbidden(Some(scope)))
        fake.refuses(Some(FakeMcpServer.Refusal(404, Vector.empty, "Not Found")))
        listed(client(fake)) ==> Left(McpError.Legacy(404))
        fake.refuses(Some(FakeMcpServer.Refusal(502, Vector.empty, "")))
        listed(client(fake)) ==> Left(McpError.Status(502))
        fake.refuses(None)
        fake.speaks(Vector("2025-11-25", "2025-06-18"))
        listed(client(fake)) ==> Left(McpError.Unsupported(Vector("2025-11-25", "2025-06-18")))
      }
    }

    test("a server that cannot be reached is Unreachable") {
      val fake = FakeMcpServer.start()
      val c = client(fake)
      fake.stop()
      c.tools() match {
        case Left(McpError.Unreachable(why)) => why.takeWhile(_ != ':') ==> "ConnectException"
        case other => throw new java.lang.AssertionError(s"not Unreachable: $other")
      }
    }

    test("an answer that has not come by Timeout on the clock is Unreachable") {
      withFake { fake =>
        fake.lists(Vector(tool("a")))
        fake.stalls(true)
        // Each reading of this clock is 61 seconds after the last, so the deadline has passed
        // by the first line of the stalled stream.
        client(fake, new Hands(step = 61000L)).tools() ==>
          Left(McpError.Unreachable("no answer within 60 seconds"))
      }
    }
  }
}
