package grit.mcp.edge

import grit.core.clock.Clock
import grit.core.edge.{DeskError, Edges, Permit, Registration, Route, ToolRequest, Variable}
import grit.core.host.Clipped
import grit.core.id.{CallSlot, ConversationId, EdgeId, PrincipalId, TurnRef, TurnSeq}
import grit.core.place.{Directory, Namespace, Place, Service}
import grit.core.tool.{Outcome, Retry, ToolName, ToolSet}
import grit.core.visibility.{
  Accounted,
  Accounts,
  Cap,
  Compartments,
  Integrity,
  Itemised,
  Part,
  Provenance,
  Readers,
  RoomLabels,
  Visibility
}
import grit.mcp.client.{Credential, FakeMcpServer, KeyFiles, McpClient, McpServer}
import grit.mcp.scope.McpScope
import grit.mcp.wire.McpCalled

import utest.*

/** [[McpTools]] against [[FakeMcpServer]]: a request run on its server, and the set advertised. */
object McpToolsTests extends TestSuite {

  private val Token = Variable("FAKE_MCP_TOKEN")

  private val Github: Service = Service.of("github").getOrElse(throw new java.lang.AssertionError())

  /** A client of `fake` as the server `github`, its answers' items read by `accounts`, reading
    * `clock`.
    */
  private def client(
      fake: FakeMcpServer,
      clock: Clock,
      accounts: Accounts[McpCalled]
  ): McpClient^{clock} = {
    val server = McpServer
      .of("github", fake.endpoint, Credential.static(Token), McpScope.Open, accounts = accounts)
      .fold(
        why => throw new java.lang.AssertionError(why),
        identity
      )
    val authorizer = Credential
      .open(server.credential, Map("FAKE_MCP_TOKEN" -> fake.token), KeyFiles.Local)
      .fold(
        why => throw new java.lang.AssertionError(why.message),
        identity
      )
    new McpClient(server, authorizer, clock)
  }

  /** The tools over `fake` as the server `github`, its answers' items read by `accounts`,
    * advertising with `advertise`.
    */
  private def toolsOver(
      fake: FakeMcpServer,
      advertise: ToolSet -> Either[DeskError, Unit] = _ => Right(()),
      accounts: Accounts[McpCalled] = Accounts.none
  ): McpTools^ = {
    val clock = Clock.system()
    new McpTools(clock, Vector(client(fake, clock, accounts)), advertise)
  }

  /** `segments` as a place under [[Github]]'s. */
  private def at(segments: String*): Place =
    Place.under(Namespace.Service, Github.name +: segments.toVector)

  /** A reader that places each answer by what it was given: under the tool's name, the call's
    * `repo`, and the answer's first text block, vouched at `vouched`.
    */
  private def placing(vouched: Integrity): Accounts[McpCalled] = new Accounts[McpCalled] {
    def items(c: McpCalled): Itemised =
      Itemised.Whole(
        Accounted.of(
          Vector(
            Provenance(
              at(c.tool, c.arguments("repo").str, c.result("content")(0)("text").str),
              vouched
            )
          )
        )
      )
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
      Set.empty,
      None
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
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        fake.answers("get_file_contents", text("# widgets"))
        val mcp = toolsOver(fake)
        val q = request("github_get_file_contents", Read)
        mcp.run(route(q), q, _ => ()) ==> Outcome.Done("# widgets")
        fake.received
          .filter(_.method.contains("tools/call"))
          .map(r => ujson.read(r.body)("params")("arguments")) ==> Vector(Read)
      }
    }

    test("a server's accounts read each answer, and its items reach the outcome") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        fake.answers("get_file_contents", text("widgets"))
        val mcp = toolsOver(fake, accounts = placing(Integrity.Outside))
        val q = request("github_get_file_contents", Read)
        mcp.run(route(q), q, _ => ()) ==> Outcome.Done(
          "widgets",
          items = Accounted.of(
            Vector(Provenance(at("get_file_contents", "r", "widgets"), Integrity.Outside))
          )
        )
      }
    }

    test("an answer whose items claim above their repository's cap is believed at the cap") {
      // The reference deployment's shape: the service untrusted, one repository declared member.
      val repository = at("get_file_contents", "r")
      val visibility = Visibility
        .of(
          Compartments.Shipped,
          RoomLabels.Public,
          Vector.empty,
          Vector.empty,
          caps = Vector(Cap(Github.place, Integrity.Untrusted), Cap(repository, Integrity.Member))
        )
        .fold(why => throw new java.lang.AssertionError(why.toString), identity)
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        fake.answers("get_file_contents", text("widgets"))
        val mcp = toolsOver(fake, accounts = placing(Integrity.Authority))
        val q = request("github_get_file_contents", Read)
        visibility.believed(Github, mcp.run(route(q), q, _ => ()).accounted) ==> Integrity.Member
      }
    }

    test(
      "a server's cut answer is a done whose parts are its items and whose text is the head; " +
        "an unplaced one is a failed call saying so"
    ) {
      val parts = Vector(
        Part(Provenance(at("r", "a"), Integrity.Member), Readers.AtPlace, "alpha"),
        Part(Provenance(at("r", "b"), Integrity.Outside), Readers.AtPlace, "beta")
      )
      val cutting = new Accounts[McpCalled] {
        def items(c: McpCalled): Itemised =
          if (c.tool == "search_code") Itemised.Parts("found:", parts)
          else Itemised.Unplaced("the file's folder could not be looked up")
      }
      withFake { fake =>
        fake.lists(
          Vector(FakeMcpServer.github("search_code"), FakeMcpServer.github("get_file_contents"))
        )
        fake.answers("search_code", text("alpha and beta"))
        fake.answers("get_file_contents", text("widgets"))
        val mcp = toolsOver(fake, accounts = cutting)
        val search = request("github_search_code", ujson.Obj("query" -> "x"))
        val read = request("github_get_file_contents", Read)
        Vector(
          mcp.run(route(search), search, _ => ()),
          mcp.run(route(read), read, _ => ())
        ) ==> Vector(
          Outcome.Done("found:", parts = Some(parts)),
          Outcome.Failed(
            "github answered, but grit could not tell who may see the answer, so none of it " +
              "is shown: the file's folder could not be looked up"
          )
        )
      }
    }

    test(
      "a cut answer over Clipped's limits keeps the leading parts that fit, saying only that " +
        "some were left out"
    ) {
      val wide = "x" * 2000
      val parts = Vector.tabulate(30)(i =>
        Part(Provenance(at("r", s"f$i"), Integrity.Member), Readers.AtPlace, wide)
      )
      val cutting = new Accounts[McpCalled] {
        def items(c: McpCalled): Itemised = Itemised.Parts("found:", parts)
      }
      val head = "found:\n\n[Some of this answer was left out to fit; call again asking for " +
        "fewer, or for the next page, to see the rest.]"
      // Each kept part adds its line: a newline and its text.
      val fit = (Clipped.MaxBytes - head.length) / (wide.length + 1)
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("search_code")))
        fake.answers("search_code", text("thirty files"))
        val mcp = toolsOver(fake, accounts = cutting)
        val search = request("github_search_code", ujson.Obj("query" -> "x"))
        mcp.run(route(search), search, _ => ()) ==> Outcome.Done(
          head,
          parts = Some(parts.take(fit))
        )
      }
    }

    test("an answer the tool marks isError is Failed with its text") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        fake.answers(
          "get_file_contents",
          ujson.Obj.from(text("no such path").value ++ Seq("isError" -> ujson.True))
        )
        val mcp = toolsOver(fake)
        val q = request("github_get_file_contents", Read)
        mcp.run(route(q), q, _ => ()) ==> Outcome.Failed("no such path")
      }
    }

    test("an exchange that fails is Failed, naming the server and why") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        val mcp = toolsOver(fake)
        val _ = mcp.offered()
        fake.refuses(
          Some(FakeMcpServer.Refusal(401, Vector("WWW-Authenticate" -> "Bearer"), ""))
        )
        val q = request("github_get_file_contents", Read)
        mcp.run(route(q), q, _ => ()) ==>
          Outcome.Failed("github refused grit's token (HTTP 401; it asks: Bearer)")
      }
    }

    test("an exchange that fails tells the operator's log why, its tool named") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        val mcp = toolsOver(fake)
        val _ = mcp.offered()
        fake.refuses(Some(FakeMcpServer.Refusal(503, Vector.empty, "")))
        val heard = Vector.newBuilder[String]
        val q = request("github_get_file_contents", Read)
        val _ = mcp.run(route(q), q, heard += _)
        heard.result() ==> Vector("github_get_file_contents: github answered HTTP 503")
      }
    }

    test("a tool no list offers is Failed naming those that are, and no call is sent") {
      withFake { fake =>
        fake.lists(
          Vector(FakeMcpServer.github("get_file_contents"), FakeMcpServer.issueWrite)
        )
        val mcp = toolsOver(fake)
        val q = request("github_issue_write", ujson.Obj())
        mcp.run(route(q), q, _ => ()) ==> Outcome.Failed(
          "There is no tool named `github_issue_write`; the tools are `github_get_file_contents`."
        )
        fake.count("tools/call") ==> 0
      }
    }

    test("where a request was routed plays no part: a directory's runs as the service's") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        fake.answers("get_file_contents", text("same"))
        val mcp = toolsOver(fake)
        val dir = Place.of(Directory.of("/repo").getOrElse(throw new java.lang.AssertionError()))
        Vector(
          request("github_get_file_contents", Read, dir),
          request("github_get_file_contents", Read)
        )
          .map(q => mcp.run(route(q), q, _ => ())) ==> Vector(
          Outcome.Done("same"),
          Outcome.Done("same")
        )
      }
    }

    test("each tool is advertised as listed: free, rerun when cut short, its schema as sent") {
      withFake { fake =>
        val snap = FakeMcpServer.github("get_file_contents")
        fake.lists(Vector(snap))
        toolsOver(fake).offered().map(_.tools) ==> Right(
          Vector(
            ToolSet.Entry(
              ToolName("github_get_file_contents"),
              "Get file or directory contents: Get the contents of a file or directory from a GitHub repository",
              snap("inputSchema"),
              asks = false,
              Retry.Rerun
            )
          )
        )
      }
    }

    test(
      "a tool whose schema declares `to`, which only a writing tool's destinations may name, is neither offered nor run"
    ) {
      withFake { fake =>
        val aiming = ujson.Obj.from(FakeMcpServer.github("get_me").value)
        aiming("inputSchema") = ujson.Obj(
          "type" -> "object",
          "properties" -> ujson.Obj("to" -> ujson.Obj("type" -> "string"))
        )
        fake.lists(Vector(FakeMcpServer.github("get_file_contents"), aiming))
        val mcp = toolsOver(fake)
        mcp.offered().map(_.tools.map(e => ToolName.value(e.name))) ==>
          Right(Vector("github_get_file_contents"))
        val q = request("github_get_me", ujson.Obj("to" -> "x"))
        mcp.run(route(q), q, _ => ()) ==> Outcome.Failed(
          "There is no tool named `github_get_me`; the tools are `github_get_file_contents`."
        )
      }
    }

    test("the set is advertised when the lists change, and not again while they do not") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        val adverts = new Adverts()
        val mcp = toolsOver(fake, adverts.advertise)
        val q = request("github_get_file_contents", Read)
        mcp.run(route(q), q, _ => ())
        mcp.run(route(q), q, _ => ())
        fake.lists(
          Vector(FakeMcpServer.github("get_file_contents"), FakeMcpServer.github("get_me"))
        )
        mcp.run(route(q), q, _ => ())
        adverts.names ==> Vector(
          Vector("github_get_file_contents"),
          Vector("github_get_file_contents", "github_get_me")
        )
      }
    }

    test("a set the desk failed to advertise is advertised again on the next run") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        val adverts = new Adverts(Left(DeskError("down")))
        val mcp = toolsOver(fake, adverts.advertise)
        val q = request("github_get_file_contents", Read)
        mcp.run(route(q), q, _ => ())
        mcp.run(route(q), q, _ => ())
        mcp.run(route(q), q, _ => ())
        adverts.names ==> Vector(
          Vector("github_get_file_contents"),
          Vector("github_get_file_contents")
        )
      }
    }
  }
}
