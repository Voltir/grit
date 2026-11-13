package grit.commands.routes

import scala.concurrent.duration.*

import grit.commands.caller.{Caller, Callers}
import grit.commands.declare.{HoldingWatch, OwnCommand, Ran}
import grit.core.clock.Clock
import grit.core.edge.{EdgeWatch, PluginCommands}
import grit.core.place.Place
import grit.core.tool.ToolName
import grit.edge.http.{Hosts, Request, Response, Status, Verb}
import grit.mcp.wire.{CallResult, Listing, McpTool, Posted, Rpc, ServerInfo}

/** The commands edge's routes: each request admitted by `hosts`, its caller asked of `callers`,
  * and run in `room`, dated by `clock`, through `commands` or, by its name, one of `own`; `tools`,
  * every offered command as `/mcp` lists it.
  */
private[commands] final class Routes private (
    commands: PluginCommands,
    watch: EdgeWatch,
    clock: Clock^,
    room: Place,
    hosts: Hosts,
    callers: Callers,
    own: Vector[OwnCommand],
    holding: Option[HoldingWatch],
    server: ServerInfo,
    tools: Vector[ujson.Obj]
) {

  /** `request` answered, running nothing first when `hosts` do not admit it, or it has no caller
    * ([[Callers.refused]]), each 403 whatever its path. At `/command/<name>`, a POST runs the
    * command `name` with its body, a JSON object (an empty body: none), as [[Answered.command]]
    * answers it; another verb 405, a body that is no JSON object 400, each running nothing. At
    * `/mcp`, one MCP message ([[Rpc.received]]): `tools/list` lists every offered plugin command
    * ([[McpTool.described]]), a guarded one by the plugin's own schema; `tools/call` runs one as
    * `/command` would, its answer a result as [[Answered.call]] reads it, and names no added
    * command. At `/wait`, a POST waits on `watch` as [[Waited.watchOf]] reads its
    * body, blocking for the whole wait, answered as [[Waited.watched]] says; another verb 405, a
    * body it does not read 400, each waiting on nothing. Any other path 404.
    */
  def answer(request: Request): Response =
    if (!hosts.admit(request))
      Response.text(Status.Forbidden, "refused: the request's Host or Origin is not this edge's")
    else
      callers.from(request) match {
        case Left(why) => Callers.refused(why)
        case Right(caller) =>
          request.path match {
            case Vector(Routes.Command, raw) => commanded(request, caller, raw)
            case Vector(Routes.Mcp) => mcp(request, caller)
            case Vector(Routes.Wait) => waited(request, caller)
            case _ =>
              Response.text(Status.NotFound, s"no route /${request.path.mkString("/")}")
          }
      }

  private def commanded(request: Request, caller: Caller, raw: String): Response =
    if (request.verb != Verb.Post)
      Response.text(Status.NotAllowed(Vector(Verb.Post)), "post a command; nothing else runs one")
    else
      ToolName.of(raw) match {
        case Left(_) => Response.text(Status.NotFound, s"no command is named $raw")
        case Right(name) =>
          Routes.arguments(request.body) match {
            case Left(why) => Response.text(Status.BadRequest, why)
            case Right(args) =>
              val (status, text) = run(caller, name, args)
              Response.text(status, text)
          }
      }

  private def mcp(request: Request, caller: Caller): Response =
    Rpc.received(request, server) match {
      case Posted.Answered(response) => response
      case list: Posted.List =>
        list.answered(
          if (list.cursor.nonEmpty) Listing.NoSuchCursor
          else Listing.Tools(tools, None)
        )
      case call: Posted.Call =>
        call.answered(
          ToolName
            .of(call.tool)
            .toOption
            .filter(n => commands.offered.exists(_.schema.name == ToolName.value(n))) match {
            case None => CallResult.NoSuchTool
            case Some(name) => Answered.call(run(caller, name, call.arguments))
          }
        )
    }

  private def waited(request: Request, caller: Caller): Response =
    if (request.verb != Verb.Post)
      Response.text(Status.NotAllowed(Vector(Verb.Post)), "post a watch; nothing else waits")
    else
      Routes
        .arguments(request.body)
        .flatMap(Waited.watchOf(_, caller.worker, holding, clock.now())) match {
        case Left(why) => Response.text(Status.BadRequest, why)
        case Right((what, within)) =>
          val (status, text) = Waited.watched(
            what,
            within,
            holding,
            watch.until(caller.account, room, what, within)
          )
          Response.text(status, text)
      }

  /** `name` run for `caller` with `args`: a deployment's own command of that name, guarding or
    * added, in place of a plugin's.
    */
  private def run(caller: Caller, name: ToolName, args: ujson.Obj) =
    own.find(_.name == name) match {
      case Some(o) if !caller.believed.dominates(o.least) => Answered.below(name, o.least)
      case Some(o) => Answered.command(o.run(ran(caller), args))
      case None => Answered.command(ran(caller)(name, args))
    }

  private def ran(caller: Caller): Ran = (name, args) =>
    commands.run(caller.account, room, caller.believed, name, args, clock.now())
}

private[commands] object Routes {

  /** The routes, or why not: the first offered command `/mcp` cannot list
    * ([[McpTool.described]]).
    */
  def of(
      commands: PluginCommands,
      watch: EdgeWatch,
      clock: Clock^,
      room: Place,
      hosts: Hosts,
      callers: Callers,
      own: Vector[OwnCommand],
      holding: Option[HoldingWatch],
      server: ServerInfo
  ): Either[String, Routes^{commands, watch, clock}] =
    commands.offered
      .foldLeft[Either[String, Vector[ujson.Obj]]](Right(Vector.empty)) { (done, l) =>
        done.flatMap(ts => McpTool.described(l.schema, l.reads).map(ts :+ _))
      }
      .left
      .map(why => s"an offered command cannot be listed at /mcp: $why")
      .map(tools =>
        new Routes(commands, watch, clock, room, hosts, callers, own, holding, server, tools)
      )

  /** `command`: the first segment of a command's path, its name the second. */
  val Command: String = "command"

  /** `mcp`: where MCP messages are posted. */
  val Mcp: String = "mcp"

  /** `wait`: where a watch is posted. */
  val Wait: String = "wait"

  /** 5 minutes: how long a watch waits when its body names no `within`; a client gone meanwhile
    * holds its place among [[EdgeWatch.Most]] until then.
    */
  val Within: FiniteDuration = 5.minutes

  /** `body` as a command's arguments: an empty body none, otherwise a JSON object, or why not. */
  def arguments(body: String): Either[String, ujson.Obj] =
    if (body.trim.isEmpty) Right(ujson.Obj())
    else
      scala.util.Try(ujson.read(body)).toOption match {
        case Some(o: ujson.Obj) => Right(o)
        case _ => Left("the body is no JSON object of the command's arguments")
      }

}
