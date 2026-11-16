package grit.commands.routes

import grit.core.edge.Adapter
import grit.core.id.Harness
import grit.edge.http.{Status, Verb}

import utest.*
import RouteFixtures.*

/** `GET /join/<harness>`: the join script of the harness's adapter. */
object JoinRouteTests extends TestSuite {

  private def joined: Routes^ =
    routed(adapters =
      Vector(Adapter(Harness.of("made-up").fold(sys.error, identity), "#!/bin/sh\n"))
    )

  val tests = Tests {
    test("only a GET is answered: any other method is 405 allowing GET, naming why") {
      val answered = Vector(Verb.Post, Verb.Put, Verb.Delete, Verb.Head).map { verb =>
        val got = joined.answer(request(verb, "/join/made-up"))
        (got.status, got.body)
      }
      answered ==> Vector.fill(4)(
        (Status.NotAllowed(Vector(Verb.Get)), "get a join script; it is only read")
      )
    }
  }
}
