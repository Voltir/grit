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
        asked("/holder?harness=made-up&dir=/work/grit"),
        asked("/holder?harness=made-up&dir=/work/grit&session=s-1&type=coder-opus")
      ) ==> (
        (Status.Ok, "desktop/grit-dcfe7476"),
        (Status.Ok, "desktop/grit-dcfe7476/coder-opus/5704e74050f4")
      )
    }

    test("an agent's empty type, sent with or without '=', is answered as its hash alone") {
      Vector(
        "/holder?harness=made-up&dir=/work/grit&session=s-1&type=",
        "/holder?harness=made-up&dir=/work/grit&session=s-1&type"
      ).map(asked(_)) ==> Vector.fill(2)(
        (Status.Ok, "desktop/grit-dcfe7476/e3b0c442/5704e74050f4")
      )
    }

    test("a session holding '/' is 400, since an agent's session is written so") {
      asked("/holder?harness=made-up&dir=/work/grit&session=s-1/a-1&type=coder") ==> (
        Status.BadRequest,
        "session: holds '/', which parts an agent's session from the session starting it: s-1/a-1"
      )
    }

    test("a query it does not read is 400 naming why") {
      Vector(
        "/holder?harness=made-up&dir=/work/grit&slot=x",
        "/holder?harness=made-up&dir=/work/grit&dir=/work/b",
        "/holder?harness=made-up&dir=/work/grit&type=coder-opus",
        "/holder?harness=made-up&dir=/work/grit&session=s-1",
        "/holder?harness=made-up&dir=work/grit",
        "/holder?dir=/work/grit",
        "/holder?harness=Made-Up&dir=/work/grit"
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
          "harness: a harness's name holds 'M', not a lowercase letter, a digit or '-': Made-Up"
        )
      )
    }

    test("only a GET is answered, and it runs nothing") {
      val commands = scripted(Vector(listed("holder", reads = false)))(Unrun)
      val r = routed(commands)
      val get = r.answer(request(Verb.Get, "/holder?harness=made-up&dir=/work/grit"))
      val post = r.answer(request(Verb.Post, "/holder?harness=made-up&dir=/work/grit"))
      (get.status, post.status, commands.ran) ==>
        (Status.Ok, Status.NotAllowed(Vector(Verb.Get)), Vector.empty)
    }
  }
}
