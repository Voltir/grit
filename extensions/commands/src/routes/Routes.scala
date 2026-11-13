package grit.commands.routes

import grit.commands.caller.{Caller, Callers}
import grit.commands.declare.{OwnCommand, Ran}
import grit.core.clock.Clock
import grit.core.edge.PluginCommands
import grit.core.place.Place
import grit.core.tool.ToolName
import grit.edge.http.{Hosts, Request, Response, Status, Verb}
import grit.mcp.wire.{CallResult, Listing, McpTool, Posted, Rpc, ServerInfo}

/** The commands edge's routes: each request admitted by `hosts`, its caller asked of `callers`,
  * and run in `room`, dated by `clock`, through `commands` or, by its name, one of `own`.
  */
private[commands] final class Routes(
    commands: PluginCommands,
    clock: Clock^,
    room: Place,
    hosts: Hosts,
    callers: Callers,
    own: Vector[OwnCommand],
    server: ServerInfo
) {

  /** `request` answered, running nothing first when `hosts` do not admit it (403). At
    * `/command/<name>`, a POST runs the command `name` with its body, a JSON object (an empty
    * body: none), as [[Answered.command]] answers it; another verb 405, a body that is no JSON
    * object 400, no caller 403 ([[Callers.refused]]), each running nothing. At `/mcp`, one MCP
    * message ([[Rpc.received]]) from a caller: `tools/list` lists every offered plugin command
    * ([[McpTool.described]]), a guarded one by the plugin's own schema; `tools/call` runs one as
    * `/command` would, its answer a result as [[Answered.call]] reads it, and names no added
    * command. Any other path 404.
    */
  def answer(request: Request): Response =
    if (!hosts.admit(request))
      Response.text(Status.Forbidden, "refused: the request's Host or Origin is not this edge's")
    else
      request.path match {
        case Vector(Routes.Command, raw) => commanded(request, raw)
        case Vector(Routes.Mcp) => mcp(request)
        case _ =>
          Response.text(Status.NotFound, s"no route /${request.path.mkString("/")}")
      }

  private def commanded(request: Request, raw: String): Response =
    if (request.verb != Verb.Post)
      Response.text(Status.NotAllowed(Vector(Verb.Post)), "post a command; nothing else runs one")
    else
      ToolName.of(raw) match {
        case Left(_) => Response.text(Status.NotFound, s"no command is named $raw")
        case Right(name) =>
          Routes.arguments(request.body) match {
            case Left(why) => Response.text(Status.BadRequest, why)
            case Right(args) =>
              callers.from(request) match {
                case Left(why) => Callers.refused(why)
                case Right(caller) =>
                  val (status, text) = run(caller, name, args)
                  Response.text(status, text)
              }
          }
      }

  private def mcp(request: Request): Response =
    callers.from(request) match {
      case Left(why) => Callers.refused(why)
      case Right(caller) =>
        Rpc.received(request, server) match {
          case Posted.Answered(response) => response
          case list: Posted.List =>
            list.answered(
              if (list.cursor.nonEmpty) Listing.NoSuchCursor
              else
                Listing.Tools(
                  commands.offered.flatMap(l => McpTool.described(l.schema, l.reads).toOption),
                  None
                )
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

  /** `command`: the first segment of a command's path, its name the second. */
  val Command: String = "command"

  /** `mcp`: where MCP messages are posted. */
  val Mcp: String = "mcp"

  /** `body` as a command's arguments: an empty body none, otherwise a JSON object, or why not. */
  def arguments(body: String): Either[String, ujson.Obj] =
    if (body.trim.isEmpty) Right(ujson.Obj())
    else
      scala.util.Try(ujson.read(body)).toOption match {
        case Some(o: ujson.Obj) => Right(o)
        case _ => Left("the body is no JSON object of the command's arguments")
      }

}
