package grit.commands.edge

import java.io.{InputStream, OutputStream}
import java.net.Socket
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import grit.commands.caller.{Caller, Callers}
import grit.commands.declare.OwnCommand
import grit.commands.routes.RouteFixtures.*
import grit.core.admin.InMemoryAdministration
import grit.core.clock.SetClock
import grit.core.document.InMemoryDocuments
import grit.core.edge.{
  Attesting,
  EdgeContracts,
  EdgeRefusal,
  EdgeStores,
  EdgeWatch,
  InMemoryAcknowledgements,
  InMemoryDeliveries,
  InMemoryEdges,
  InMemoryJoins,
  InMemoryOrigins,
  PluginCommands
}
import grit.core.files.NoIntake
import grit.core.id.{AttesterName, EdgeName}
import grit.core.identity.{Realm, Standing, TestAccounts, Vouched}
import grit.core.inbox.InMemoryInbox
import grit.core.plugin.CommandOutcome
import grit.core.review.InMemoryReviews
import grit.core.spend.Budget
import grit.core.store.{InMemoryVoucher, TestJot}
import grit.core.tool.ToolName
import grit.core.visibility.{Integrity, Visibility}
import grit.edge.http.{Hosts, Listen}

import utest.*

/** [[CommandsEdge]]: what `of` and `open` refuse, and the edge served over a real socket. */
object CommandsEdgeTests extends TestSuite {

  private val realm: Realm = got(Realm.of("slack", "T1"))
  private val loopback: Listen = got(Listen.parse("127.0.0.1:0"))
  private val done = ToolName("done")

  private def edge(
      at: Listen = loopback,
      hosts: Hosts = Hosts.loopback,
      callers: Callers = Owners,
      own: Vector[OwnCommand] = Vector.empty
  ): Either[String, CommandsEdge] =
    CommandsEdge.of(
      EdgeName("commands"),
      at,
      hosts,
      EdgeContracts.Room,
      realm,
      AttesterName("desk"),
      callers,
      Server,
      own,
      None
    )

  private def stores(commands: PluginCommands, voucher: InMemoryVoucher): EdgeStores^ = {
    val inbox = InMemoryInbox.fresh(Budget(java.time.ZoneOffset.UTC, None))
    val jot = new TestJot
    EdgeStores(
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
      jot,
      new InMemoryEdges,
      new Attesting(voucher, jot, _ => ()),
      new NoIntake,
      commands = commands,
      watch = EdgeWatch.Refusing
    )
  }

  /** `raw`, an HTTP/1.1 request, sent to `port` on this machine: the status line and the body. */
  private def sent(port: Int, raw: String): (String, String) = {
    val socket = new Socket("127.0.0.1", port)
    try {
      val out: OutputStream = socket.getOutputStream
      out.write(raw.getBytes(UTF_8))
      out.flush()
      val in: InputStream = socket.getInputStream
      val whole = new String(in.readAllBytes(), UTF_8)
      (whole.takeWhile(_ != '\r'), whole.split("\r\n\r\n", 2).lift(1).getOrElse(""))
    } finally socket.close()
  }

  private def post(port: Int, host: String, target: String, body: String) =
    sent(
      port,
      s"POST $target HTTP/1.1\r\nHost: $host\r\nConnection: close\r\n" +
        s"Content-Length: ${body.getBytes(UTF_8).length}\r\n\r\n$body"
    )

  val tests = Tests {
    test(
      "of refuses two own commands of one name, a caller outside its realm, and no credential beyond loopback"
    ) {
      val twice = Vector(
        OwnCommand.added(ToolName("hello"), Integrity.Member, (ran, a) => ran(done, a)),
        OwnCommand.guarding(ToolName("hello"), Integrity.Member, (ran, a) => ran(done, a))
      )
      val outsider =
        Callers.owner(Caller(TestAccounts.account("slack:T2/U9"), Integrity.Member, Worker))
      Vector(
        edge(own = twice),
        edge(callers = outsider),
        edge(at = got(Listen.parse("0.0.0.0:7317"))),
        edge(hosts = got(Hosts.of(Vector("localhost", "box.lan"))))
      ).map(_.left.toOption) ==> Vector(
        Some("two own commands are named hello"),
        Some("slack:T2/U9 is no account of slack:T1/"),
        Some(
          "a request with no credential is the owner's, so the edge listens on loopback alone, not 0.0.0.0:7317"
        ),
        Some(
          "a request with no credential is the owner's, so the edge admits loopback's names alone"
        )
      )
    }

    test("open refuses a guard of no offered command, and an added one a plugin offers") {
      val commands =
        scripted(Vector(listed("done", reads = false)))((_, _) => Right(CommandOutcome.Done("")))
      def opened(own: OwnCommand) =
        got(edge(own = Vector(own)))
          .open(
            stores(commands, InMemoryVoucher.none()),
            Map.empty,
            new SetClock(Now),
            _ => ()
          ) match {
          case Left(refusal) => Some(refusal)
          case Right(open) => open.close(); None
        }
      (
        opened(OwnCommand.guarding(ToolName("absent"), Integrity.Member, (ran, a) => ran(done, a))),
        opened(OwnCommand.added(done, Integrity.Member, (ran, a) => ran(done, a)))
      ) ==> (
        Some(EdgeRefusal.Refused("the own command absent guards no offered command")),
        Some(EdgeRefusal.Refused("the own command done is added, but a plugin offers it"))
      )
    }

    test(
      "opened, it vouches its callers and answers on its socket, refusing another site's Host, until closed"
    ) {
      val commands = scripted(Vector(listed("done", reads = false))) { (_, args) =>
        Right(CommandOutcome.Done(s"did ${args("text").str}"))
      }
      val voucher = new InMemoryVoucher(Set(realm), Set.empty, Visibility.Shipped)
      val log = new ConcurrentLinkedQueue[String]()
      val open = got(edge())
        .open(
          stores(commands, voucher),
          Map.empty,
          new SetClock(Now),
          l => { val _ = log.add(l) }
        ) match {
        case Right(o) => o
        case Left(r) => throw new java.lang.AssertionError(r.toString)
      }
      val port =
        try {
          val port = log.asScala.collectFirst {
            case l if l.startsWith("commands: serving on http://127.0.0.1:") =>
              l.stripPrefix("commands: serving on http://127.0.0.1:").toInt
          }
          val p = port.getOrElse(throw new java.lang.AssertionError(s"no address logged: $log"))
          (
            post(p, s"127.0.0.1:$p", "/command/done", """{"text":"it"}"""),
            post(p, s"rebound.example:$p", "/command/done", """{"text":"it"}""")
          ) ==> (
            ("HTTP/1.1 200 OK", "did it"),
            ("HTTP/1.1 403 Forbidden", "refused: the request's Host or Origin is not this edge's")
          )
          voucher.vouched ==> Vector(Vouched(EdgeContracts.Reader, Standing.Full(None)))
          p
        } finally open.close()
      assert(scala.util.Try(sent(port, "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n")).isFailure)
    }
  }
}
