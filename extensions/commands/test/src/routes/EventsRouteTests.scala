package grit.commands.routes

import grit.commands.worker.{Holders, Worktrees}
import grit.core.edge.{EdgeContracts, EdgeSessions, ScriptedSessions}
import grit.core.id.{Harness, SessionId}
import grit.core.place.Directory
import grit.core.session.Event
import grit.edge.http.{Status, Verb}

import utest.*
import RouteFixtures.*

/** `POST /events`: a harness's lines recorded on their session as the caller, each refused named. */
object EventsRouteTests extends TestSuite {

  private val MadeUp: Harness = got(Harness.of("made-up"))
  private val At = Now.minusSeconds(30)

  private def line(n: String, fields: (String, ujson.Value)*): String =
    ujson.write(
      ujson.Obj.from(
        Seq[(String, ujson.Value)](
          "session" -> "s-1",
          "dir" -> "/work/grit",
          "n" -> n,
          "at" -> At.toString,
          "event" -> "edited"
        ) ++ fields
      )
    )

  private def posted(
      script: ScriptedSessions.Script,
      body: String,
      target: String = "/events?harness=made-up",
      vouched: Boolean = true
  ) = {
    val got = routed(script = script, vouched = vouched).answer(request(Verb.Post, target, body))
    (got.status, got.body)
  }

  private def recorded(script: ScriptedSessions.Script) = script.called.collect {
    case ScriptedSessions.Call.Recorded(by, room, batches) => (by, room, batches)
  }

  val tests = Tests {
    test(
      "the lines read are recorded on their session as the caller, in the edge's room, and each refused is named; sent again, none is"
    ) {
      val script = new ScriptedSessions.Script
      val body = Vector(line("1"), line("0"), line("2")).mkString("\n")
      val first = posted(script, body)
      val again = posted(script, body)
      val refused =
        ujson.Arr(ujson.Obj("line" -> 2, "why" -> "n: an event's ordinal is above 0: 0"))
      val dir = got(Directory.of("/work/grit"))
      (first, again, recorded(script).headOption) ==> (
        (Status.Ok, ujson.write(ujson.Obj("recorded" -> 2, "refused" -> refused))),
        (Status.Ok, ujson.write(ujson.Obj("recorded" -> 0, "refused" -> refused))),
        Some(
          (
            Owner.account,
            EdgeContracts.Room,
            Vector(
              EdgeSessions.Batch(
                got(SessionId.of(MadeUp, "s-1")),
                None,
                Worktrees.at(Worker, dir),
                Some(Holders.of(Worker, dir, None)),
                Vector(1L, 2L).map(n => got(Event.of(n, At, Event.Kind.Edited, Event.Shape.Empty)))
              )
            )
          )
        )
      )
    }

    test("a post refused whole is 400, and records nothing") {
      val script = new ScriptedSessions.Script
      (
        posted(script, line("1"), "/events"),
        posted(script, line("1"), "/events?harness=made-up&session=s-1"),
        posted(script, line("1"), "/events?harness=grit"),
        posted(
          script,
          Vector(line("1"), line("2", "agent" -> "a-1", "type" -> "t")).mkString("\n")
        ),
        recorded(script)
      ) ==> (
        (Status.BadRequest, "no harness"),
        (Status.BadRequest, "no parameter session is read here; these are: harness"),
        (Status.BadRequest, "the harness grit is grit's own: only its turns record its sessions"),
        (
          Status.BadRequest,
          "line 2 is session made-up:s-1/a-1, not made-up:s-1: post each session's lines apart"
        ),
        Vector.empty
      )
    }

    test("a post whose every line is refused is 200 naming each, and records nothing") {
      val script = new ScriptedSessions.Script
      (posted(script, Vector(line("0"), line("x")).mkString("\n")), recorded(script)) ==> (
        (
          Status.Ok,
          ujson.write(
            ujson.Obj(
              "recorded" -> 0,
              "refused" -> ujson.Arr(
                ujson.Obj("line" -> 1, "why" -> "n: an event's ordinal is above 0: 0"),
                ujson.Obj("line" -> 2, "why" -> "n: a decimal string, not \"x\"")
              )
            )
          )
        ),
        Vector.empty
      )
    }

    test("an account no person is linked to is 403, and a database that cannot be written 503") {
      val script = new ScriptedSessions.Script
      val stranger = posted(script, line("1"), vouched = false)
      script.failing(Some("down"))
      (stranger, posted(script, line("1"))) ==> (
        (
          Status.Forbidden,
          s"nothing was recorded: no person is linked to ${grit.core.identity.Account.written(Owner.account)}"
        ),
        (Status.Unavailable, "nothing was recorded, and resending is safe: down")
      )
    }

    test("only a POST is answered: a GET records nothing") {
      val script = new ScriptedSessions.Script
      val get = routed(script = script).answer(request(Verb.Get, "/events?harness=made-up"))
      (get.status, script.called) ==> (Status.NotAllowed(Vector(Verb.Post)), Vector.empty)
    }
  }
}
