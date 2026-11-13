package grit.commands.routes

import grit.commands.declare.OwnCommand
import grit.core.clock.SetClock
import grit.core.edge.{CommandError, EdgeContracts, ScriptedCommands}
import grit.core.plugin.CommandOutcome
import grit.core.tool.ToolName
import grit.core.visibility.Integrity
import grit.edge.http.{Hosts, Verb}

import utest.*
import RouteFixtures.*

/** `POST /mcp`: the offered commands as MCP tools, each call run as `/command` runs it. */
object McpRouteTests extends TestSuite {

  private def answering(name: ToolName, args: ujson.Value) = ToolName.value(name) match {
    case "done" => Right(CommandOutcome.Done(s"did ${args("text").str}"))
    case "look" => Right(CommandOutcome.Done("looked"))
    case _ => Left(CommandError.Unavailable("unreached"))
  }

  private def offered: ScriptedCommands =
    scripted(Vector(listed("done", reads = false), listed("look", reads = true)))(answering)

  private def routes(commands: ScriptedCommands, own: Vector[OwnCommand]): Routes^ =
    got(
      Routes.of(
        commands,
        Unwatched,
        new SetClock(Now),
        EdgeContracts.Room,
        Hosts.loopback,
        Owners,
        own,
        None,
        Server
      )
    )

  /** `method` with `params` posted to `/mcp` in 2025-11-25: the answer's status and JSON body. */
  private def rpc(
      r: Routes^,
      method: String,
      params: ujson.Obj,
      more: Vector[(String, String)] = Vector.empty
  ): (Status, ujson.Value) = {
    val body =
      ujson.write(ujson.Obj("jsonrpc" -> "2.0", "id" -> 7, "method" -> method, "params" -> params))
    val headers = Vector(
      "Host" -> "localhost:7317",
      "Content-Type" -> "application/json",
      "MCP-Protocol-Version" -> "2025-11-25"
    ) ++ more
    val got = r.answer(request(Verb.Post, "/mcp", body, headers))
    (got.status, scala.util.Try(ujson.read(got.body)).getOrElse(ujson.Str(got.body)))
  }

  private type Status = grit.edge.http.Status

  /** A call's result: its text and whether it is an error; or the JSON-RPC error's message. */
  private def called(answer: (Status, ujson.Value)): Either[String, (String, Boolean)] =
    answer._2.objOpt.flatMap(_.get("result")) match {
      case Some(result) =>
        Right((result("content").arr.head("text").str, result("isError").bool))
      case None =>
        Left(answer._2.objOpt.flatMap(_.get("error")).fold(answer._2.toString)(_("message").str))
    }

  private def call(r: Routes^, name: String) =
    called(
      rpc(r, "tools/call", ujson.Obj("name" -> name, "arguments" -> ujson.Obj("text" -> "it")))
    )

  val tests = Tests {
    test("tools/list lists each offered command with whether it only reads, an added one not") {
      val added =
        OwnCommand.added(ToolName("hello"), Integrity.Member, (ran, a) => ran(ToolName("done"), a))
      val (_, listed) = rpc(routes(offered, Vector(added)), "tools/list", ujson.Obj())
      listed.objOpt
        .flatMap(_.get("result"))
        .fold(Vector.empty[(String, Boolean)])(
          _("tools").arr.toVector.map(t => (t("name").str, t("annotations")("readOnlyHint").bool))
        ) ==>
        Vector(("done", false), ("look", true))
    }

    test(
      "tools/call answers the command's text; a refusal is an error; an unknown or added name no tool"
    ) {
      val commands =
        scripted(Vector(listed("done", reads = false), listed("refuse", reads = false))) {
          (name, args) =>
            if (ToolName.value(name) == "done")
              Right(CommandOutcome.Done(s"did ${args("text").str}"))
            else Right(CommandOutcome.Refused("not today"))
        }
      val added =
        OwnCommand.added(ToolName("hello"), Integrity.Member, (ran, a) => ran(ToolName("done"), a))
      val r = routes(commands, Vector(added))
      Vector(call(r, "done"), call(r, "refuse"), call(r, "hello"), call(r, "absent")) ==> Vector(
        Right(("did it", false)),
        Right(("not today", true)),
        Left("no tool is named hello"),
        Left("no tool is named absent")
      )
    }

    test("tools/call goes through a guard: below its least nothing runs, at it the guard runs") {
      val strict = offered
      val above = OwnCommand.guarding(
        ToolName("done"),
        Integrity.Authority,
        (ran, a) => ran(ToolName("done"), a)
      )
      val kept = offered
      val at = OwnCommand.guarding(
        ToolName("done"),
        Integrity.Member,
        (ran, _) => ran(ToolName("done"), ujson.Obj("text" -> "guarded"))
      )
      (call(routes(strict, Vector(above)), "done"), call(routes(kept, Vector(at)), "done")) ==> (
        Right(("done is given only by a caller believed at authority", true)),
        Right(("did guarded", false))
      )
      (strict.ran.size, kept.ran.size) ==> (0, 1)
    }

    test("a request carrying a credential is refused before any message is read") {
      val commands = offered
      rpc(
        routes(commands, Vector.empty),
        "tools/list",
        ujson.Obj(),
        Vector("Authorization" -> "Bearer t0ken")
      )._1 ==>
        grit.edge.http.Status.Forbidden
      commands.ran ==> Vector.empty
    }
  }
}
