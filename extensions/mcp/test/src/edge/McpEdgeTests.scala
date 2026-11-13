package grit.mcp.edge

import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import grit.core.admin.InMemoryAdministration
import grit.core.clock.Clock
import grit.core.document.InMemoryDocuments
import grit.core.edge.InMemoryJoins
import grit.core.edge.{
  Attesting,
  Desk,
  DeskError,
  Desks,
  EdgeRefusal,
  EdgeStores,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  InMemoryOrigins,
  Permit,
  ServedEdge,
  ToolRequest,
  Variable
}
import grit.core.files.NoIntake
import grit.core.host.Argv
import grit.core.id.{CallSlot, ConversationId, EdgeName, PrincipalId, TurnRef, TurnSeq}
import grit.core.inbox.InMemoryInbox
import grit.core.place.{Directory, Place, Service}
import grit.core.review.InMemoryReviews
import grit.core.spend.Budget
import grit.core.store.{InMemoryVoucher, Jot, Origin, StoreError, Tx}
import grit.core.tool.{Outcome, Retry, ToolName, ToolSet}
import grit.core.visibility.Subject
import grit.dbos.sql.TestTx
import grit.edge.desk.PlaceFragments
import grit.mcp.client.{Credential, FakeMcpServer, FakeStdioServer, McpServer, McpTransport}
import grit.mcp.scope.{Bound, McpScope}

import utest.*

/** [[McpEdge]] opened over in-memory stores against [[FakeMcpServer]]. */
object McpEdgeTests extends TestSuite {

  private object FakeJot extends Jot {
    def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
      body(using
        TestTx.fake
      )
  }

  private val Token = Variable("FAKE_MCP_TOKEN")

  private val Github: Service = Service.of("github").getOrElse(throw new java.lang.AssertionError())

  private def server(
      fake: FakeMcpServer,
      name: String = "github",
      allow: Set[String] = Set.empty,
      scope: McpScope = McpScope.Open
  ): McpServer[McpTransport.Http] =
    McpServer
      .of(name, fake.endpoint, Credential.static(Token), scope, allow)
      .fold(why => throw new java.lang.AssertionError(why), identity)

  /** Desks that cannot be registered: the database cannot be reached. */
  private object Unreachable extends Desks {
    def register(principal: PrincipalId, places: Set[Place]): Either[DeskError, Desk^] =
      Left(DeskError("the database could not be reached"))
  }

  /** What [[McpEdge]] opened over `fake` did: its result, the edges it registered in, and its
    * log; closed after. Its desks are those edges, or [[Unreachable]] when `reachable` is false.
    */
  private def opened(
      fake: FakeMcpServer,
      env: Map[String, String],
      reachable: Boolean = true,
      allow: Set[String] = Set.empty,
      scope: McpScope = McpScope.Open,
      instructions: Option[String] = None
  ): (Either[EdgeRefusal, Unit], InMemoryEdges, Vector[String]) = {
    val edges = new InMemoryEdges
    val inbox = InMemoryInbox.fresh(Budget(java.time.ZoneOffset.UTC, None))
    val stores = EdgeStores(
      inbox,
      InMemoryAdministration.none(),
      InMemoryJoins.none(),
      inbox.principals,
      new InMemoryDeliveries,
      inbox.ledger,
      new InMemoryAcknowledgements,
      InMemoryReviews.over(inbox),
      new InMemoryOrigins(inbox.conversations),
      (new InMemoryDocuments).reviews.of,
      FakeJot,
      if (reachable) edges else Unreachable,
      new Attesting(InMemoryVoucher.none(), FakeJot, _ => ()),
      new NoIntake
    )
    val log = new ConcurrentLinkedQueue[String]()
    val edge = McpEdge
      .serving(Github, Vector(server(fake, allow = allow, scope = scope)), instructions)
      .fold(why => throw new java.lang.AssertionError(why), identity)
    val result = edge.open(stores, env, Clock.system(), line => { val _ = log.add(line) }) match {
      case Left(refusal) => Left(refusal)
      case Right(open) => Right(open.close())
    }
    (result, edges, log.asScala.toVector)
  }

  private val Scout: Service =
    Service.of("scout").fold(why => throw new java.lang.AssertionError(why), identity)

  /** scout as a program [[FakeStdioServer]] stands in for, setting `root` to the asking
    * conversation's directory.
    */
  private val scout: McpServer[McpTransport.Stdio] =
    Argv
      .of("/opt/scout", "mcp")
      .flatMap(McpServer.stdio("scout", _, McpScope.Open, directoryArgument = Some("root")))
      .fold(why => throw new java.lang.AssertionError(why), identity)

  /** A request of `tool` from `conversation`'s first turn, call `index`, addressed to scout. */
  private def asked(conversation: String, index: Int): ToolRequest = {
    val turn = TurnRef(ConversationId(conversation), TurnSeq.First)
    ToolRequest(
      CallSlot.of(turn, 0, index).getOrElse(throw new java.lang.AssertionError()),
      ToolRequest.Protocol,
      turn.conversationId,
      Scout.place,
      PrincipalId.Grit,
      ToolName("scout_show"),
      Permit.Free,
      Retry.Rerun,
      ujson.Obj("symbols" -> ujson.Arr("A")),
      Set.empty,
      None
    )
  }

  private def withFake[A](body: FakeMcpServer => A): A = {
    val fake = FakeMcpServer.start()
    try body(fake)
    finally fake.stop()
  }

  val tests = Tests {
    test("the edge is named for its service, needs each server's token once, and is never asked") {
      withFake { fake =>
        McpEdge
          .serving(Github, Vector(server(fake), server(fake, "other")))
          .map(e => (EdgeName.value(e.name), e.needs, e.asks)) ==>
          Right(("github", Vector(Token), ServedEdge.Asks.Never))
      }
    }

    test("an edge of no servers, of two with one name, or with blank instructions is refused") {
      withFake { fake =>
        Vector(
          McpEdge.serving(Github, Vector.empty).map(_ => ()),
          McpEdge.serving(Github, Vector(server(fake), server(fake))).map(_ => ()),
          McpEdge.serving(Github, Vector(server(fake)), Some(" ")).map(_ => ())
        ) ==> Vector(
          Left("the MCP edge for service:github names no server"),
          Left("two MCP servers are named github"),
          Left("the instructions declared for service:github are blank")
        )
      }
    }

    test("opened, it advertises at the service's place the tools the server offers, logged") {
      withFake { fake =>
        val snap = FakeMcpServer.github("get_file_contents")
        fake.lists(Vector(snap, FakeMcpServer.issueWrite))
        val (result, edges, log) = opened(fake, Map("FAKE_MCP_TOKEN" -> fake.token))
        val offered = ToolSet
          .of(
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
          .fold(d => throw new java.lang.AssertionError(d.toString), identity)
        (result, edges.adverts.toVector.map((at, advert) => (at._2, advert.tools)), log) ==> (
          Right(()),
          Vector((Github.place, offered.id)),
          Vector(
            "MCP server github: issue_write is not offered: it is not marked read-only",
            "service:github: serving github_get_file_contents"
          )
        )
      }
    }

    test("opened with instructions, it advertises them as the place's Place layer") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        val text = "Our code is the GitHub repository octocat/Hello-World."
        val (_, edges, _) =
          opened(fake, Map("FAKE_MCP_TOKEN" -> fake.token), instructions = Some(text))
        edges.adverts.values.toVector.map(_.instructions) ==>
          Vector(PlaceFragments.declared(Github.place, text).toSeq.toVector.map(_.id))
      }
    }

    test(
      "the tools an allowlist leaves out are logged in one line, its unlisted names in another"
    ) {
      withFake { fake =>
        fake.lists(
          Vector("get_file_contents", "list_commits", "search_code").map(FakeMcpServer.github) :+
            FakeMcpServer.issueWrite
        )
        val allow = Set("get_file_contents", "issue_write", "list_tags", "get_me")
        opened(fake, Map("FAKE_MCP_TOKEN" -> fake.token), allow = allow)._3 ==> Vector(
          "MCP server github: issue_write is not offered: it is not marked read-only",
          "MCP server github: 2 listed tools are not on its allowlist: list_commits, search_code",
          "MCP server github: its allowlist names tools it does not list: get_me, list_tags",
          "service:github: serving github_get_file_contents"
        )
      }
    }

    test("the tools its scope cannot hold are logged in one line, after those not allowed") {
      withFake { fake =>
        fake.lists(
          Vector("get_file_contents", "search_code", "search_issues", "get_me")
            .map(FakeMcpServer.github)
        )
        val scope = Bound
          .of("owner" -> "example-org", "repo" -> "widgets")
          .fold(why => throw new java.lang.AssertionError(why), McpScope.within(_))
        val allow = Set("get_file_contents", "search_code", "search_issues")
        opened(fake, Map("FAKE_MCP_TOKEN" -> fake.token), allow = allow, scope = scope)._3 ==>
          Vector(
            "MCP server github: 1 listed tool is not on its allowlist: get_me",
            "MCP server github: 2 listed tools are outside its scope: search_code, search_issues",
            "service:github: serving github_get_file_contents"
          )
      }
    }

    test(
      "open is refused when the server refuses the token, naming its variable; nothing registers"
    ) {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        fake.refuses(
          Some(FakeMcpServer.Refusal(401, Vector("WWW-Authenticate" -> "Bearer"), ""))
        )
        val (result, edges, _) = opened(fake, Map("FAKE_MCP_TOKEN" -> fake.token))
        (result, edges.live) ==> (
          Left(
            EdgeRefusal.Refused(
              "MCP server github refused the token it was given (HTTP 401; it asks: Bearer); check FAKE_MCP_TOKEN"
            )
          ),
          Map.empty
        )
      }
    }

    test("a tool declaring to is logged as skipped, and the rest offered") {
      withFake { fake =>
        val sending = ujson.Obj(
          "name" -> "send_mail",
          "inputSchema" -> ujson.Obj(
            "type" -> "object",
            "properties" -> ujson.Obj("to" -> ujson.Obj("type" -> "string"))
          ),
          "annotations" -> ujson.Obj("readOnlyHint" -> true)
        )
        fake.lists(Vector(FakeMcpServer.github("get_me"), sending))
        opened(fake, Map("FAKE_MCP_TOKEN" -> fake.token))._3 ==>
          Vector(
            "MCP server github: send_mail is not offered: it declares to, the argument grit " +
              "keeps for where a writing tool writes",
            "service:github: serving github_get_me"
          )
      }
    }

    test(
      "local logs a malformed tool alone and offers the rest; served, each call is sent its conversation's directory, and one with none is answered unsent"
    ) {
      val fake = new FakeStdioServer
      fake.lists(
        Vector(
          ujson.Obj("inputSchema" -> ujson.Obj("type" -> "object")),
          ujson.Obj(
            "name" -> "show",
            "inputSchema" -> ujson.Obj(
              "type" -> "object",
              "properties" -> ujson.Obj("root" -> ujson.Obj("type" -> "string"))
            ),
            "annotations" -> ujson.Obj("readOnlyHint" -> true)
          )
        )
      )
      fake.answers("show", "shown")
      val log = new ConcurrentLinkedQueue[String]()
      val local = McpEdge.local(
        Scout,
        scout,
        fake,
        Clock.system(),
        None,
        line => { val _ = log.add(line) }
      ) match {
        case Right(local) => local
        case Left(why) => throw new java.lang.AssertionError(why)
      }
      val edges = new InMemoryEdges
      val dir =
        Directory.of("/work/tree").fold(e => throw new java.lang.AssertionError(e), identity)
      edges.originates(ConversationId("chat"), Origin.Tui(dir, "default"))
      edges.originates(
        ConversationId("thread"),
        Origin.Slack("T1", "C1", "1.0")
      )
      local.serve(edges)
      val (chat, thread) = (asked("chat", 0), asked("thread", 1))
      val tx = TestTx.fake
      edges.dispatch(Vector(chat, thread), Tx.classification(using tx))(using tx) ==> Right(())
      val until = System.nanoTime() + 5_000_000_000L
      while (edges.answers.size < 2 && System.nanoTime() < until) Thread.sleep(20)
      local.close()
      (
        edges.adverts.keys.map(_._2).toVector,
        log.asScala.toVector.take(2),
        edges.answers.toMap.get(chat.slot),
        edges.answers.toMap.get(thread.slot),
        fake.received.filter(_("method").str == "tools/call").map(_("params")("arguments"))
      ) ==> (
        Vector(Scout.place),
        Vector(
          "MCP server scout: a listed tool is not offered: a listed tool has no name",
          "service:scout: serving scout_show"
        ),
        Some(Outcome.Done("shown")),
        Some(
          Outcome.Failed(
            "scout was not asked: it reads the directory of the conversation asking, and this " +
              "conversation works in no directory"
          )
        ),
        Vector(ujson.Obj("symbols" -> ujson.Arr("A"), "root" -> "/work/tree"))
      )
    }

    test(
      "local is refused, and its program stopped, when its server lists no tool that may be offered"
    ) {
      val fake = new FakeStdioServer
      fake.lists(
        Vector(ujson.Obj("name" -> "show", "inputSchema" -> ujson.Obj("type" -> "object")))
      )
      val refused = McpEdge.local(Scout, scout, fake, Clock.system(), None, _ => ()) match {
        case Right(local) => local.close(); Right(())
        case Left(why) => Left(why)
      }
      (refused, fake.kills) ==>
        (Left("MCP server scout lists no tool that may be offered"), 1)
    }

    test("open is refused when the server lists no tool that may be offered") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.issueWrite))
        opened(fake, Map("FAKE_MCP_TOKEN" -> fake.token))._1 ==>
          Left(EdgeRefusal.Refused("MCP server github lists no tool that may be offered"))
      }
    }

    test("open fails, saying why, when its desk cannot be registered") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        opened(fake, Map("FAKE_MCP_TOKEN" -> fake.token), reachable = false)._1 ==>
          Left(
            EdgeRefusal.Failed(
              "its desk could not be registered: the database could not be reached"
            )
          )
      }
    }

    test("open is refused, and nothing is sent, when the token is unset") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.github("get_file_contents")))
        (opened(fake, Map.empty)._1, fake.received) ==> (Left(EdgeRefusal.Missing(Token)), Vector())
      }
    }
  }
}
