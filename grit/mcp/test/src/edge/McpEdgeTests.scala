package grit.mcp.edge

import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import grit.core.edge.{EdgeRefusal, EdgeStores, InMemoryDeliveries, InMemoryEdges, Variable}
import grit.core.id.EdgeName
import grit.core.inbox.InMemoryInbox
import grit.core.place.Service
import grit.core.spend.Budget
import grit.core.store.{Jot, StoreError, Tx}
import grit.core.tool.{Retry, ToolName, ToolSet}
import grit.dbos.sql.TestTx
import grit.mcp.client.{FakeMcpServer, McpServer}

import utest.*

/** [[McpEdge]] opened over in-memory stores against [[FakeMcpServer]]. */
object McpEdgeTests extends TestSuite {

  private object FakeJot extends Jot {
    def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] = body(using
      TestTx.fake
    )
  }

  private val Token = Variable("FAKE_MCP_TOKEN")

  private val Github: Service = Service.of("github").getOrElse(throw new java.lang.AssertionError())

  private def server(fake: FakeMcpServer, name: String = "github"): McpServer =
    McpServer
      .of(name, fake.endpoint, Token)
      .fold(why => throw new java.lang.AssertionError(why), identity)

  /** What [[McpEdge]] opened over `fake` did: its result, the edges it registered in, and its
    * log; closed after.
    */
  private def opened(
      fake: FakeMcpServer,
      env: Map[String, String]
  ): (Either[EdgeRefusal, Unit], InMemoryEdges, Vector[String]) = {
    val edges = new InMemoryEdges
    val inbox = InMemoryInbox.fresh(Budget(java.time.ZoneOffset.UTC, None))
    val stores = EdgeStores(inbox, inbox.principals, new InMemoryDeliveries, FakeJot, edges)
    val log = new ConcurrentLinkedQueue[String]()
    val edge = McpEdge
      .serving(Github, Vector(server(fake)))
      .fold(why => throw new java.lang.AssertionError(why), identity)
    val result = edge.open(stores, env, line => { val _ = log.add(line) }) match {
      case Left(refusal) => Left(refusal)
      case Right(open) => Right(open.close())
    }
    (result, edges, log.asScala.toVector)
  }

  private def withFake[A](body: FakeMcpServer => A): A = {
    val fake = FakeMcpServer.start()
    try body(fake)
    finally fake.stop()
  }

  val tests = Tests {
    test("the edge is named for its service, needs each server's token once, answers no ask") {
      withFake { fake =>
        McpEdge
          .serving(Github, Vector(server(fake), server(fake, "other")))
          .map(e => (EdgeName.value(e.name), e.needs, e.answersAsks)) ==>
          Right(("github", Vector(Token), false))
      }
    }

    test("an edge of no servers, or of two with one name, is refused") {
      withFake { fake =>
        Vector(
          McpEdge.serving(Github, Vector.empty).map(_ => ()),
          McpEdge.serving(Github, Vector(server(fake), server(fake))).map(_ => ())
        ) ==> Vector(
          Left("the MCP edge for service:github names no server"),
          Left("two MCP servers are named github")
        )
      }
    }

    test("opened, it advertises at the service's place the tools the server offers, logged") {
      withFake { fake =>
        val snap = FakeMcpServer.snap("get_file_contents")
        fake.lists(Vector(snap, FakeMcpServer.snap("issue_write")))
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

    test(
      "open is refused when the server refuses the token, naming its variable; nothing registers"
    ) {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.snap("get_file_contents")))
        fake.refuses(
          Some(FakeMcpServer.Refusal(401, Vector("WWW-Authenticate" -> "Bearer"), ""))
        )
        val (result, edges, _) = opened(fake, Map("FAKE_MCP_TOKEN" -> fake.token))
        (result, edges.live) ==> (
          Left(
            EdgeRefusal.Refused(
              "MCP server github refused grit's token (HTTP 401; it asks: Bearer); check FAKE_MCP_TOKEN"
            )
          ),
          Map.empty
        )
      }
    }

    test("open is refused when the server lists no tool grit may offer") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.snap("issue_write")))
        opened(fake, Map("FAKE_MCP_TOKEN" -> fake.token))._1 ==>
          Left(EdgeRefusal.Refused("MCP server github lists no tool grit may offer"))
      }
    }

    test("open is refused, and nothing is sent, when the token is unset") {
      withFake { fake =>
        fake.lists(Vector(FakeMcpServer.snap("get_file_contents")))
        (opened(fake, Map.empty)._1, fake.received) ==> (Left(EdgeRefusal.Missing(Token)), Vector())
      }
    }
  }
}
