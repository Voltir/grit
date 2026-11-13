package grit.commands.edge

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import grit.commands.declare.Adapter
import grit.commands.routes.RouteFixtures.*
import grit.core.clock.SetClock
import grit.core.edge.{EdgeContracts, ScriptedSessions}
import grit.core.id.{AttesterName, EdgeName, Harness, SessionId}
import grit.core.identity.Realm
import grit.core.session.{Session, SessionDigest}
import grit.core.store.InMemoryVoucher
import grit.core.visibility.Visibility
import grit.edge.http.{Hosts, Listen}

import utest.*
import EdgeFixtures.*

/** A harness no code here names, `pi-like`, joining the edge and working through it with plain
  * HTTP: what the edge serves any harness, given its adapter.
  */
object MadeUpHarnessTests extends TestSuite {

  private val PiLike: Harness = got(Harness.of("pi-like"))
  private val Join = "#!/bin/sh\n# sets pi-like up to post its events to the edge at $1\n"

  private def encoded(s: String) = URLEncoder.encode(s, UTF_8)

  val tests = Tests {
    test("a made-up harness joins, learns its holder, posts its events and is briefed on them") {
      val script = new ScriptedSessions.Script
      val realm = got(Realm.of("slack", "T1"))
      val log = new ConcurrentLinkedQueue[String]()
      val open = got(
        CommandsEdge.of(
          EdgeName("commands"),
          got(Listen.parse("127.0.0.1:0")),
          Hosts.loopback,
          EdgeContracts.Room,
          realm,
          AttesterName("desk"),
          Owners,
          Server,
          Vector.empty,
          None,
          Vector(Adapter(PiLike, Join))
        )
      ).open(
        stores(
          scripted(Vector.empty)(Unrun),
          new InMemoryVoucher(Set(realm), Set.empty, Visibility.Shipped),
          new ScriptedSessions(Set(Owner.account), script)
        ),
        Map.empty,
        new SetClock(Now),
        l => { val _ = log.add(l) }
      ) match {
        case Right(o) => o
        case Left(r) => throw new java.lang.AssertionError(r.toString)
      }
      try {
        val port = log.asScala
          .collectFirst {
            case l if l.startsWith("commands: serving on http://127.0.0.1:") =>
              l.stripPrefix("commands: serving on http://127.0.0.1:").toInt
          }
          .getOrElse(throw new java.lang.AssertionError(s"no address logged: $log"))
        val host = s"localhost:$port"
        val dir = encoded("/work/pi")
        val at = Now.minusSeconds(30)
        def line(n: Int, event: String, extra: String = "") =
          s"""{"session":"p-1","dir":"/work/pi","n":"$n","at":"$at","event":"$event"$extra}"""
        val events = Vector(
          line(1, "started"),
          line(2, "edited", ""","shape":{"path":"notes.md"}"""),
          line(3, "closing", ""","text":"Wrote the notes."""")
        ).mkString("\n")
        val brief = s"/brief?harness=pi-like&session=p-2&dir=$dir&start=fresh&part=digest"
        val answered = (
          get(port, host, "/join/pi-like"),
          get(port, host, "/join/claude-code"),
          get(port, host, s"/holder?harness=pi-like&dir=$dir"),
          post(port, host, "/events?harness=pi-like", events),
          get(port, host, brief)
        )
        val worked = script
          .session(got(SessionId.of(PiLike, "p-1")))
          .map(s => Session.Lately(s, Vector.empty, 0, Vector.empty, Vector.empty))
        (
          answered,
          script.called.count(_.isInstanceOf[ScriptedSessions.Call.Recorded])
        ) ==> (
          (
            ("HTTP/1.1 200 OK", Join),
            ("HTTP/1.1 404 Not Found", "no adapter is given for the harness claude-code"),
            ("HTTP/1.1 200 OK", "desktop/pi-689beccf"),
            ("HTTP/1.1 200 OK", """{"recorded":3,"refused":[]}"""),
            (
              "HTTP/1.1 200 OK",
              worked.fold("")(
                SessionDigest.of(_, SessionDigest.Window.LacksLastSaid, SessionDigest.Chars)
              )
            )
          ),
          1
        )
        assert(answered._5._2.contains("Last said: Wrote the notes."))
      } finally open.close()
    }
  }
}
