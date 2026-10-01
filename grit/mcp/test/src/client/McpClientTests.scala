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
        fake.lists(Vector(FakeMcpServer.Weather, FakeMcpServer.snap("get_me")), pageSize = 1)
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
          Vector(tool("a"), FakeMcpServer.snap("issue_write"), tool("b"), tool("c"))
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

    test("a list longer than MaxPages pages is unreadable, not followed for ever") {
      withFake { fake =>
        fake.lists(Vector.tabulate(McpClient.MaxPages + 1)(i => tool(s"t$i")), pageSize = 1)
        client(fake).tools().map(_.tools.size) ==>
          Left(McpError.Unreadable(s"tools/list ran past ${McpClient.MaxPages} pages"))
        fake.lists(Vector.tabulate(McpClient.MaxPages)(i => tool(s"t$i")), pageSize = 1)
        client(fake).tools().map(_.tools.size) ==> Right(McpClient.MaxPages)
      }
    }
  }
}
