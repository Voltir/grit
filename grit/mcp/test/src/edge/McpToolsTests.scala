package grit.mcp.edge

import grit.core.clock.Clock
import grit.core.edge.{DeskError, Edges, Permit, Registration, Route, ToolRequest, Variable}
import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Place, Service}
import grit.core.tool.{Outcome, Retry, ToolName, ToolSet}
import grit.mcp.client.{Bearer, FakeMcpServer, McpClient, McpServer}

import utest.*

/** [[McpTools]] against [[FakeMcpServer]]: a request run on its server, and the set advertised. */
object McpToolsTests extends TestSuite {

  private val Token = Variable("FAKE_MCP_TOKEN")

  private val Github: Service = Service.of("github").getOrElse(throw new java.lang.AssertionError())

  /** A client of `fake` as the server `github`. */
  private def client(fake: FakeMcpServer): McpClient^ = {
    val server = McpServer
      .of("github", fake.endpoint, Token)
      .fold(
        why => throw new java.lang.AssertionError(why),
        identity
      )
    val bearer = Bearer
      .of(Map("FAKE_MCP_TOKEN" -> fake.token), Token)
      .fold(
        why => throw new java.lang.AssertionError(why.message),
        identity
      )
    new McpClient(server, bearer, Clock.system())
  }

  /** A request of `tool` with `arguments`, addressed to `at`. */
  private def request(
      tool: String,
      arguments: ujson.Value,
      at: Place = Github.place
  ): ToolRequest = {
    val turn = TurnRef(ConversationId("c"), TurnSeq.First)
    ToolRequest(
      CallSlot.of(turn, 0, 0).getOrElse(throw new java.lang.AssertionError()),
      ToolRequest.Protocol,
      turn.conversationId,
      at,
      PrincipalId.Grit,
      ToolName.of(tool).fold(why => throw new java.lang.AssertionError(why), identity),
      Permit.Free,
      Retry.Rerun,
      arguments,
      Set.empty
    )
  }

  /** Where [[Edges.authorize]] routes `q` for an edge hosting its workspace. */
  private def route(q: ToolRequest): Route =
    Edges
      .authorize(q, Registration(EdgeId("e"), PrincipalId.Grit, Set(q.workspace)))
      .fold(r => throw new java.lang.AssertionError(r.toString), identity)

  private val Read = ujson.Obj("owner" -> "o", "repo" -> "r", "path" -> "README.md")

  private def text(t: String): ujson.Obj =
    ujson.Obj("content" -> ujson.Arr(ujson.Obj("type" -> "text", "text" -> t)))

  /** The tools advertised, and the desk's answer to each, in order. */
  private final class Adverts(answers: Either[DeskError, Unit]*) {
    // Written by the test's thread alone: McpTools advertises on the caller's thread.
    @caps.unsafe.untrackedCaptures
    var told = Vector.empty[ToolSet]

    def advertise(set: ToolSet): Either[DeskError, Unit] = {
      told = told :+ set
      answers.lift(told.size - 1).getOrElse(Right(()))
    }

    def names: Vector[Vector[String]] = told.map(_.tools.map(e => ToolName.value(e.name)))
  }

  private def withFake[A](body: FakeMcpServer => A): A = {
    val fake = FakeMcpServer.start()
    try body(fake)
    finally fake.stop()
  }

  val tests = Tests {
    test("a request runs its tool on the server, with its arguments, and answers its text") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.snap("get_file_contents")))
        fake.answers("get_file_contents", text("# actualbest"))
        val tools = new McpTools(Vector(client(fake)), _ => Right(()))
        val q = request("github_get_file_contents", Read)
        tools.run(route(q), q) ==> Outcome.Done("# actualbest")
        fake.received
          .filter(_.method.contains("tools/call"))
          .map(r => ujson.read(r.body)("params")("arguments")) ==> Vector(Read)
      }
    }

    test("an answer the tool marks isError is Failed with its text") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.snap("get_file_contents")))
        fake.answers(
          "get_file_contents",
          ujson.Obj.from(text("no such path").value ++ Seq("isError" -> ujson.True))
        )
        val tools = new McpTools(Vector(client(fake)), _ => Right(()))
        val q = request("github_get_file_contents", Read)
        tools.run(route(q), q) ==> Outcome.Failed("no such path")
      }
    }

    test("an exchange that fails is Failed, naming the server and why") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.snap("get_file_contents")))
        val tools = new McpTools(Vector(client(fake)), _ => Right(()))
        tools.offered()
        fake.refuses(
          Some(FakeMcpServer.Refusal(401, Vector("WWW-Authenticate" -> "Bearer"), ""))
        )
        val q = request("github_get_file_contents", Read)
        tools.run(route(q), q) ==>
          Outcome.Failed("github refused grit's token (HTTP 401; it asks: Bearer)")
      }
    }

    test("a tool no list offers is Failed naming those that are, and no call is sent") {
      withFake { fake =>
        fake.lists(
          Vector(FakeMcpServer.snap("get_file_contents"), FakeMcpServer.snap("issue_write"))
        )
        val tools = new McpTools(Vector(client(fake)), _ => Right(()))
        val q = request("github_issue_write", ujson.Obj())
        tools.run(route(q), q) ==> Outcome.Failed(
          "There is no tool named `github_issue_write`; the tools are `github_get_file_contents`."
        )
        fake.count("tools/call") ==> 0
      }
    }

    test("where a request was routed plays no part: a directory's runs as the service's") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.snap("get_file_contents")))
        fake.answers("get_file_contents", text("same"))
        val tools = new McpTools(Vector(client(fake)), _ => Right(()))
        val dir = Place.of(Directory.of("/repo").getOrElse(throw new java.lang.AssertionError()))
        Vector(
          request("github_get_file_contents", Read, dir),
          request("github_get_file_contents", Read)
        )
          .map(q => tools.run(route(q), q)) ==> Vector(Outcome.Done("same"), Outcome.Done("same"))
      }
    }

    test("each tool is advertised as listed: free, rerun when cut short, its schema as sent") {
      withFake { fake =>
        val snap = FakeMcpServer.snap("get_file_contents")
        fake.lists(Vector(snap))
        new McpTools(Vector(client(fake)), _ => Right(())).offered().tools ==> Vector(
          ToolSet.Entry(
            ToolName("github_get_file_contents"),
            "Get file or directory contents: Get the contents of a file or directory from a GitHub repository",
            snap("inputSchema"),
            asks = false,
            Retry.Rerun
          )
        )
      }
    }

    test("the set is advertised when the lists change, and not again while they do not") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.snap("get_file_contents")))
        val adverts = new Adverts()
        val tools = new McpTools(Vector(client(fake)), adverts.advertise)
        val q = request("github_get_file_contents", Read)
        tools.run(route(q), q)
        tools.run(route(q), q)
        fake.lists(Vector(FakeMcpServer.snap("get_file_contents"), FakeMcpServer.snap("get_me")))
        tools.run(route(q), q)
        adverts.names ==> Vector(
          Vector("github_get_file_contents"),
          Vector("github_get_file_contents", "github_get_me")
        )
      }
    }

    test("a set the desk failed to advertise is advertised again on the next run") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.snap("get_file_contents")))
        val adverts = new Adverts(Left(DeskError("down")))
        val tools = new McpTools(Vector(client(fake)), adverts.advertise)
        val q = request("github_get_file_contents", Read)
        tools.run(route(q), q)
        tools.run(route(q), q)
        tools.run(route(q), q)
        adverts.names ==> Vector(
          Vector("github_get_file_contents"),
          Vector("github_get_file_contents")
        )
      }
    }
  }
}
