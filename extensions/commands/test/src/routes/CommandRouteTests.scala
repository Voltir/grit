package grit.commands.routes

import grit.commands.caller.{Caller, Callers}
import grit.commands.declare.OwnCommand
import grit.core.clock.SetClock
import grit.core.edge.{Adapter, CommandError, EdgeContracts, ScriptedCommands}
import grit.core.either.Got.got
import grit.core.id.Harness
import grit.core.plugin.CommandOutcome
import grit.core.store.StoreError
import grit.core.tool.ToolName
import grit.core.visibility.Integrity
import grit.edge.http.{Challenge, Hosts, Status, Verb}

import utest.*
import RouteFixtures.*

/** `POST /command/<name>`: a command run as the request's caller, answered by one status table. */
object CommandRouteTests extends TestSuite {

  private val Names: Vector[String] = Vector("done", "refused", "unread", "store", "unavailable")

  private def answering(name: ToolName, args: ujson.Value) = ToolName.value(name) match {
    case "done" => Right(CommandOutcome.Done(s"did ${args("text").str}"))
    case "refused" => Right(CommandOutcome.Refused("not today"))
    case "unread" => Left(CommandError.Unread(name, "text is required"))
    case "store" => Left(CommandError.Store(name, StoreError.DatabaseError("disk full")))
    case _ => Left(CommandError.Unavailable("connection refused"))
  }

  private def routes(
      commands: ScriptedCommands,
      own: Vector[OwnCommand] = Vector.empty,
      callers: Callers = Owners
  ): Routes^ =
    got(
      Routes.of(
        commands,
        Unwatched,
        Unrecorded,
        new SetClock(Now),
        EdgeContracts.Room,
        Hosts.loopback,
        callers,
        own,
        None,
        Vector.empty,
        Server
      )
    )

  private def posted(r: Routes^, name: String, body: String = """{"text":"it"}""") = {
    val got = r.answer(request(Verb.Post, s"/command/$name", body))
    (got.status, got.body)
  }

  val tests = Tests {
    test("each outcome and refusal of a command is answered by its status, and its words") {
      val commands = scripted(Names.map(listed(_, reads = false)))(answering)
      val r = routes(commands)
      val stranger = Caller(EdgeContracts.Stranger, Integrity.Member, Machine)
      (Names :+ "absent").map(posted(r, _)) :+ posted(r, "done", "[1]") :+
        posted(routes(commands, callers = Callers.owner(stranger)), "done") ==> Vector(
          (Status.Ok, "did it"),
          (Status.Conflict, "not today"),
          (Status.BadRequest, "text is required"),
          (Status.Failed, "nothing was kept: DatabaseError(disk full)"),
          (Status.Unavailable, "the database is unavailable: connection refused"),
          (Status.NotFound, "no command is named absent"),
          (Status.BadRequest, "the body is no JSON object of the command's arguments"),
          (Status.Forbidden, "no person is linked to slack:T1/U-stranger")
        )
    }

    test("a GET and a request from another site each run nothing") {
      val commands = scripted(Vector(listed("done", reads = false)))(answering)
      val r = routes(commands)
      Vector(
        r.answer(request(Verb.Get, "/command/done")),
        r.answer(
          request(Verb.Post, "/command/done", headers = Vector("Host" -> "rebound.example:7317"))
        )
      ).map(_.status) ==> Vector(Status.NotAllowed(Vector(Verb.Post)), Status.Forbidden)
      commands.ran ==> Vector.empty
    }

    test("a credential is refused before the command's name, the body or an MCP message is read") {
      val commands = scripted(Vector(listed("done", reads = false)))(answering)
      val r = routes(commands)
      val credentialed = Vector("Host" -> "localhost:7317", "Authorization" -> "Bearer t0ken")
      Vector(
        r.answer(request(Verb.Post, "/command/Absent", headers = credentialed)),
        r.answer(request(Verb.Post, "/command/done", "[]", headers = credentialed)),
        r.answer(
          request(
            Verb.Post,
            "/mcp",
            """{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}""",
            headers = credentialed
          )
        )
      ).map(_.status) ==> Vector.fill(3)(Status.Unauthorized(Challenge.Refused))
      commands.ran ==> Vector.empty
    }

    test("a caller below an own command's least is refused, and nothing runs") {
      val commands = scripted(Vector(listed("done", reads = false)))(answering)
      val guard = OwnCommand.guarding(
        ToolName("done"),
        Integrity.Authority,
        (ran, a) => ran(ToolName("done"), a)
      )
      posted(routes(commands, Vector(guard)), "done") ==>
        (Status.Forbidden, "done is given only by a caller believed at authority")
      commands.ran ==> Vector.empty
    }

    test(
      "a person's own credential is given an own command their agents' is refused, and each runs at its own integrity"
    ) {
      val commands = scripted(Vector("done", "refused").map(listed(_, reads = false)))(answering)
      val done = ToolName("done")
      val guard = OwnCommand.guarding(done, Integrity.Authority, (ran, a) => ran(done, a))
      val r = routes(commands, Vector(guard), Credentialed)
      def sent(text: String, name: String) = {
        val got = r.answer(
          request(Verb.Post, s"/command/$name", """{"text":"it"}""", headers = bearing(text))
        )
        (got.status, got.body)
      }
      Vector(
        sent(OwnText, "done"),
        sent(AgentText, "done"),
        sent(OwnText, "refused"),
        sent(AgentText, "refused")
      ) ==> Vector(
        (Status.Ok, "did it"),
        (Status.Forbidden, "done is given only by a caller believed at authority"),
        (Status.Conflict, "not today"),
        (Status.Conflict, "not today")
      )
      commands.ran.map(c => (c.by, ToolName.value(c.name), c.believed)) ==> Vector(
        (Owner.account, "done", Integrity.Authority),
        (Owner.account, "refused", Integrity.Authority),
        (Owner.account, "refused", Integrity.Member)
      )
    }

    test(
      "with credentials, a request bearing none is 401 at every path, /join and an unknown one included, running nothing"
    ) {
      val commands = scripted(Vector(listed("done", reads = false)))(answering)
      val r = routed(
        commands,
        adapters = Vector(Adapter(got(Harness.of("made-up")), "#!/bin/sh\n")),
        callers = Credentialed
      )
      Vector(
        request(Verb.Post, "/command/done", """{"text":"it"}"""),
        request(Verb.Post, "/mcp", "{}"),
        request(Verb.Post, "/wait", "{}"),
        request(Verb.Get, "/holder?harness=made-up&dir=/work/grit"),
        request(Verb.Get, "/brief?harness=made-up&session=s-1&dir=/work/grit&start=fresh"),
        request(Verb.Post, "/events?harness=made-up", ""),
        request(Verb.Get, "/join/made-up"),
        request(Verb.Get, "/nowhere")
      ).map(r.answer(_).status) ==> Vector.fill(8)(Status.Unauthorized(Challenge.Asked))
      commands.ran ==> Vector.empty
    }

    test("an own command runs in place of a plugin's, and what it runs runs as the caller") {
      val commands = scripted(Vector(listed("done", reads = false)))(answering)
      val done = ToolName("done")
      val guard = OwnCommand.guarding(
        done,
        Integrity.Member,
        (ran, a) => ran(done, ujson.Obj("text" -> s"guarded ${a("text").str}"))
      )
      val added = OwnCommand.added(ToolName("hello"), Integrity.Member, (ran, a) => ran(done, a))
      val r = routes(commands, Vector(guard, added))
      (posted(r, "done"), posted(r, "hello")) ==> (
        (Status.Ok, "did guarded it"),
        (Status.Ok, "did it")
      )
      commands.ran.map(c => (c.by, c.room, c.believed, c.name, c.args, c.at)) ==> Vector(
        (
          Owner.account,
          EdgeContracts.Room,
          Integrity.Member,
          done,
          ujson.Obj("text" -> "guarded it"),
          Now
        ),
        (Owner.account, EdgeContracts.Room, Integrity.Member, done, ujson.Obj("text" -> "it"), Now)
      )
    }
  }

}
