package grit.commands.routes

import grit.commands.declare.Adapter
import grit.core.id.Harness
import grit.edge.http.{Status, Verb}

import utest.*
import RouteFixtures.*

/** The routes that read no query: `/command`, `/mcp`, `/wait` and `/join` each refuse one. */
object QueryRouteTests extends TestSuite {
  private val Call =
    """{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"note","arguments":{"text":"x"}}}"""

  val tests: Tests = Tests {
    test("a query parameter a route reads none of is 400 naming it, and runs nothing") {
      val commands = scripted(Vector(listed("note", reads = false)))(Unrun)
      val r = routed(
        commands,
        adapters = Vector(Adapter(got(Harness.of("made-up")), "#!/bin/sh\n"))
      )
      Vector(
        request(Verb.Post, "/command/note?x=1", """{"text":"x"}"""),
        request(Verb.Post, "/mcp?x=1", Call),
        request(Verb.Post, "/wait?x=1", "{}"),
        request(Verb.Get, "/join/made-up?x")
      ).map(r.answer(_)).map(a => (a.status, a.body)) :+ commands.ran ==> Vector(
        (Status.BadRequest, "no parameter x is read here; this route reads none"),
        (Status.BadRequest, "no parameter x is read here; this route reads none"),
        (Status.BadRequest, "no parameter x is read here; this route reads none"),
        (Status.BadRequest, "no parameter x is read here; this route reads none"),
        Vector.empty
      )
    }
  }
}
