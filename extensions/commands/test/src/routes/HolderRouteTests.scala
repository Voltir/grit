package grit.commands.routes

import grit.edge.http.{Status, Verb}

import utest.*
import RouteFixtures.*

/** `GET /holder`: the holder a session, or an agent it starts, works as on the caller's worker. */
object HolderRouteTests extends TestSuite {

  private def asked(target: String, verb: Verb = Verb.Get) = {
    val got = routed().answer(request(verb, target))
    (got.status, got.body)
  }

  val tests = Tests {
    test("a session's holder, and an agent's, are answered as text on the caller's worker") {
      (
        asked("/holder?harness=claude-code&dir=/work/grit"),
        asked("/holder?harness=claude-code&dir=/work/grit&session=s-1&type=coder-opus")
      ) ==> (
        (Status.Ok, "desktop/grit-dcfe7476"),
        (Status.Ok, "desktop/grit-dcfe7476/coder-opus/0e77e4b3dbc2")
      )
    }

    test("a query it does not read is 400 naming why") {
      Vector(
        "/holder?harness=claude-code&dir=/work/grit&slot=x",
        "/holder?harness=claude-code&dir=/work/grit&dir=/work/b",
        "/holder?harness=claude-code&dir=/work/grit&type=coder-opus",
        "/holder?harness=claude-code&dir=/work/grit&session=s-1",
        "/holder?harness=claude-code&dir=work/grit",
        "/holder?dir=/work/grit",
        "/holder?harness=Claude&dir=/work/grit"
      ).map(asked(_)) ==> Vector(
        (
          Status.BadRequest,
          "no parameter slot is read here; these are: harness, dir, session, type"
        ),
        (Status.BadRequest, "dir is sent twice"),
        (
          Status.BadRequest,
          "type and session come together: an agent's type, and the session starting it"
        ),
        (
          Status.BadRequest,
          "type and session come together: an agent's type, and the session starting it"
        ),
        (Status.BadRequest, "dir: not an absolute path: work/grit"),
        (Status.BadRequest, "no harness"),
        (
          Status.BadRequest,
          "harness: a harness's name holds 'C', not a lowercase letter, a digit or '-': Claude"
        )
      )
    }

    test("only a GET is answered, and it runs nothing") {
      val commands = scripted(Vector(listed("holder", reads = false)))(Unrun)
      val r = routed(commands)
      val get = r.answer(request(Verb.Get, "/holder?harness=claude-code&dir=/work/grit"))
      val post = r.answer(request(Verb.Post, "/holder?harness=claude-code&dir=/work/grit"))
      (get.status, post.status, commands.ran) ==>
        (Status.Ok, Status.NotAllowed(Vector(Verb.Get)), Vector.empty)
    }
  }
}
