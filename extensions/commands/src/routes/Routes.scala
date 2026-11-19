package grit.commands.routes

import scala.concurrent.duration.*

import grit.commands.caller.{Caller, Callers}
import grit.commands.declare.{HoldingWatch, OwnCommand, Ran}
import grit.core.clock.Clock
import grit.core.edge.Adapter
import grit.core.edge.{EdgeSessions, EdgeWatch, PluginCommands}
import grit.core.id.{ClaimHolder, Harness}
import grit.core.json.Fields
import grit.core.place.Place
import grit.core.tool.ToolName
import grit.edge.http.{Hosts, Request, Response, Status, Verb}
import grit.mcp.wire.{CallResult, Listing, McpTool, Posted, Rpc, ServerInfo}

/** The commands edge's routes: each request admitted by `hosts`, its caller asked of `callers`,
  * and run in `room`, dated by `clock`, through `commands` or, by its name, one of `own`; a
  * harness's sessions recorded and briefed through `sessions`, and its join script one of
  * `adapters`; `tools`, every offered command as `/mcp` lists it.
  */
private[commands] final class Routes private (
    commands: PluginCommands,
    watch: EdgeWatch,
    sessions: EdgeSessions,
    clock: Clock^,
    room: Place,
    hosts: Hosts,
    callers: Callers,
    own: Vector[OwnCommand],
    holding: Option[HoldingWatch],
    adapters: Vector[Adapter],
    server: ServerInfo,
    tools: Vector[ujson.Obj]
) {

  /** `request` answered: admitted by `hosts`, its caller read, then routed by its path, no route
    * seeing its `Authorization` header; what each route answers: [[grit.commands.edge.CommandsEdge]].
    */
  def answer(request: Request): Response =
    if (!hosts.admit(request))
      Response.text(Status.Forbidden, "refused: the request's Host or Origin is not this edge's")
    else
      callers.from(request) match {
        case Left(why) => why.refused
        case Right(caller) => routed(request.without("Authorization"), caller)
      }

  /** `request` answered as `caller`'s, by its path. */
  private def routed(request: Request, caller: Caller): Response =
    request.path match {
      case Vector(Routes.Command, raw) => unqueried(request)(commanded(request, caller, raw))
      case Vector(Routes.Mcp) => unqueried(request)(mcp(request, caller))
      case Vector(Routes.Wait) => unqueried(request)(waited(request, caller))
      case Vector(Briefing.Holder) => holder(request, caller)
      case Vector(Briefing.Brief) => brief(request, caller)
      case Vector(Recording.Events) => events(request, caller)
      case Vector(Routes.Join, harness) => unqueried(request)(joined(request, harness))
      case _ =>
        Response.text(Status.NotFound, s"no route /${request.path.mkString("/")}")
    }

  /** `answer`, unless `request` sends a query parameter, which a route reading none refuses. */
  private def unqueried(request: Request)(answer: => Response): Response =
    Queried.of(request, Vector.empty).fold(Response.text(Status.BadRequest, _), _ => answer)

  /** `answer` for `verb`; otherwise 405 allowing `verb` alone, saying `refusal`. */
  private def only(request: Request, verb: Verb, refusal: String)(answer: => Response): Response =
    if (request.verb != verb) Response.text(Status.NotAllowed(Vector(verb)), refusal) else answer

  private def commanded(request: Request, caller: Caller, raw: String): Response =
    only(request, Verb.Post, "post a command; nothing else runs one") {
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
            case Some(name) => RouteReply.call(run(caller, name, call.arguments))
          }
        )
    }

  private def holder(request: Request, caller: Caller): Response =
    only(request, Verb.Get, "get a holder; nothing else reads one") {
      Briefing
        .holderOf(request, caller.machine)
        .fold(
          Response.text(Status.BadRequest, _),
          h => Response.text(Status.Ok, ClaimHolder.value(h))
        )
    }

  private def brief(request: Request, caller: Caller): Response =
    only(request, Verb.Get, "get a brief; nothing else reads one") {
      Briefing.briefOf(request, caller.machine) match {
        case Left(why) => Response.text(Status.BadRequest, why)
        case Right(asked) =>
          val now = clock.now()
          val (status, text) = Briefing.briefed(
            asked,
            sessions.briefed(caller.account, room, asked.holder, asked.session, now),
            now
          )
          Response.text(status, text)
      }
    }

  private def events(request: Request, caller: Caller): Response =
    only(request, Verb.Post, "post event lines; nothing else records them") {
      Recording.batchOf(request, caller.machine, clock.now()) match {
        case Left(why) => Response.text(Status.BadRequest, why)
        case Right(batched) =>
          Recording.recorded(batched, b => sessions.record(caller.account, room, Vector(b)))
      }
    }

  private def joined(request: Request, harness: String): Response =
    only(request, Verb.Get, "get a join script; it is only read") {
      adapters
        .find(a => Harness.value(a.harness) == harness)
        .fold(Response.text(Status.NotFound, s"no adapter is given for the harness $harness"))(a =>
          Response.text(Status.Ok, a.join)
        )
    }

  private def waited(request: Request, caller: Caller): Response =
    only(request, Verb.Post, "post a watch; nothing else waits") {
      Routes
        .arguments(request.body)
        .left
        .map(Status.BadRequest -> _)
        .flatMap(Waited.watchOf(_, caller.machine, holding, ran(caller), clock.now())) match {
        case Left((status, why)) => Response.text(status, why)
        case Right((what, within)) =>
          val (status, text) = Waited.watched(
            what,
            within,
            holding,
            watch.until(caller.account, room, what, within)
          )
          Response.text(status, text)
      }
    }

  /** `name` run for `caller` with `args`: a deployment's own command of that name, guarding or
    * added, in place of a plugin's.
    */
  private def run(caller: Caller, name: ToolName, args: ujson.Obj) =
    own.find(_.name == name) match {
      case Some(o) if !caller.believed.dominates(o.least) => RouteReply.below(name, o.least)
      case Some(o) => RouteReply.command(o.run(ran(caller), args))
      case None => RouteReply.command(ran(caller)(name, args))
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
      sessions: EdgeSessions,
      clock: Clock^,
      room: Place,
      hosts: Hosts,
      callers: Callers,
      own: Vector[OwnCommand],
      holding: Option[HoldingWatch],
      adapters: Vector[Adapter],
      server: ServerInfo
  ): Either[String, Routes^{commands, watch, sessions, clock}] =
    commands.offered
      .foldLeft[Either[String, Vector[ujson.Obj]]](Right(Vector.empty)) { (done, l) =>
        done.flatMap(ts => McpTool.described(l.schema, l.reads).map(ts :+ _))
      }
      .left
      .map(why => s"an offered command cannot be listed at /mcp: $why")
      .map(tools =>
        new Routes(
          commands,
          watch,
          sessions,
          clock,
          room,
          hosts,
          callers,
          own,
          holding,
          adapters,
          server,
          tools
        )
      )

  /** `command`: the first segment of a command's path, its name the second. */
  val Command: String = "command"

  /** `mcp`: where MCP messages are posted. */
  val Mcp: String = "mcp"

  /** `wait`: where a watch is posted. */
  val Wait: String = "wait"

  /** `join`: the first segment of a join script's path, its harness the second. */
  val Join: String = "join"

  /** 5 minutes: how long a watch waits when its body names no `within`; a client gone meanwhile
    * holds its place among [[EdgeWatch.Most]] until then.
    */
  val Within: FiniteDuration = 5.minutes

  /** `body` as a command's arguments: an empty body none, otherwise a JSON object, or why not. */
  def arguments(body: String): Either[String, ujson.Obj] =
    if (body.trim.isEmpty) Right(ujson.Obj())
    else
      Fields.objectOf(body).toRight("the body is no JSON object of the command's arguments")

}
