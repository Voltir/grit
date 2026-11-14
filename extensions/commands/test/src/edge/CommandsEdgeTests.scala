package grit.commands.edge

import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import grit.commands.caller.{Caller, Callers}
import grit.commands.declare.OwnCommand
import grit.commands.routes.RouteFixtures.*
import grit.core.clock.SetClock
import grit.core.edge.Adapter
import grit.core.edge.{EdgeContracts, EdgeRefusal, PluginCommands}
import grit.core.id.{AttesterName, EdgeName, Harness}
import grit.core.identity.{Account, Realm, Standing, TestAccounts, Vouched}
import grit.core.plugin.CommandOutcome
import grit.core.provider.ToolSchema
import grit.core.store.{InMemoryVoucher, LastWord, StoreError, Tx, Voucher}
import grit.core.tool.ToolName
import grit.core.visibility.{Integrity, Visibility}
import grit.edge.http.{Hosts, Listen}

import utest.*
import EdgeFixtures.*

/** [[CommandsEdge]]: what `of` and `open` refuse, and the edge served over a real socket. */
object CommandsEdgeTests extends TestSuite {

  private val realm: Realm = got(Realm.of("slack", "T1"))
  private val loopback: Listen = got(Listen.parse("127.0.0.1:0"))
  private val done = ToolName("done")

  private def edge(
      at: Listen = loopback,
      hosts: Hosts = Hosts.loopback,
      callers: Callers = Owners,
      own: Vector[OwnCommand] = Vector.empty,
      adapters: Vector[Adapter] = Vector.empty
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
      None,
      adapters
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

    test("of refuses two adapters of one harness") {
      val madeUp = got(Harness.of("made-up"))
      edge(adapters = Vector(Adapter(madeUp, "one"), Adapter(madeUp, "two"))).left.toOption ==>
        Some("two adapters are given for the harness made-up")
    }

    test(
      "open refuses a guard of no offered command, and an added one a plugin offers, before vouching"
    ) {
      val commands =
        scripted(Vector(listed("done", reads = false)))((_, _) => Right(CommandOutcome.Done("")))
      val voucher = new InMemoryVoucher(Set(realm), Set.empty, Visibility.Shipped)
      def opened(own: OwnCommand) =
        got(edge(own = Vector(own)))
          .open(
            stores(commands, voucher),
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
      voucher.vouched ==> Vector.empty
    }

    test("open refuses an offered command that /mcp cannot list") {
      val odd = PluginCommands.Listed(ToolSchema("odd", "Runs odd.", ujson.Arr()), reads = true)
      val commands = scripted(Vector(odd))((_, _) => Right(CommandOutcome.Done("")))
      got(edge()).open(
        stores(commands, InMemoryVoucher.none()),
        Map.empty,
        new SetClock(Now),
        _ => ()
      ) match {
        case Left(refusal) =>
          refusal ==> EdgeRefusal.Refused(
            "an offered command cannot be listed at /mcp: odd's parameters are not a JSON object"
          )
        case Right(open) =>
          open.close()
          throw new java.lang.AssertionError("opened with a command /mcp cannot list")
      }
    }

    test("open fails when a caller's account cannot be vouched, and listens on nothing") {
      val unreadable = new Voucher {
        def realms: Set[Realm] = Set(realm)
        def vouch(v: Vouched)(using Tx^) = Left(StoreError.DatabaseError("down"))
        def lastWord(account: Account)(using Tx^): Either[StoreError, LastWord] =
          Left(StoreError.DatabaseError("down"))
        def lastWords(r: Realm)(using Tx^) = Left(StoreError.DatabaseError("down"))
      }
      val log = new ConcurrentLinkedQueue[String]()
      got(edge()).open(
        stores(scripted(Vector.empty)((_, _) => Right(CommandOutcome.Done(""))), unreadable),
        Map.empty,
        new SetClock(Now),
        l => { val _ = log.add(l) }
      ) match {
        case Left(refusal) =>
          refusal ==> EdgeRefusal.Failed(
            "slack:T1/U-reader could not be vouched: down"
          )
          log.asScala.toVector ==> Vector.empty
        case Right(open) =>
          open.close()
          throw new java.lang.AssertionError("opened over a voucher that cannot be read")
      }
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
