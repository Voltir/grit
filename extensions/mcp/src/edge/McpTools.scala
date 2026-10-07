package grit.mcp.edge

import java.util.concurrent.atomic.AtomicReference

import grit.core.clock.Clock
import grit.core.edge.{DeskError, Route, ToolRequest}
import grit.core.tool.{
  Args,
  Gate,
  Hosted,
  Outcome,
  Retry,
  Tool,
  ToolName,
  ToolSet,
  ToolSpec,
  Toolbox,
  Writes
}
import grit.edge.{Run, Tools}
import grit.mcp.client.McpClient
import grit.mcp.wire.McpTool

/** The requests an edge is sent, run by calling `clients`' servers, wherever they were routed:
  * a request's tool is found by name in the servers' lists as they stand ([[McpClient.tools]],
  * listed again when stale), and run as [[Run.request]] runs a tool, its arguments the JSON
  * object sent. Its answer is [[Outcome.Done]], or [[Outcome.Failed]] with the tool's text
  * when it marks it `isError`, or naming the server and the [[grit.mcp.wire.McpError]] when
  * the exchange fails. Before each run, the tools are told to `advertise` when they differ
  * from those it last took (the first time included); one it refused is told again next run.
  * `clients` read `clock` for their lists' freshness.
  */
// `clock` is a parameter, rather than a capture-set parameter over `clients`, so the class's
// captures are in its signature (docs/capture-checking.md, "A capset parameter a class's body
// alone uses").
final class McpTools(
    clock: Clock,
    clients: Vector[McpClient^{clock}],
    advertise: ToolSet => Either[DeskError, Unit]
) extends Tools {

  // The set `advertise` last took. Replaced whole; two runs that both find it changed both
  // advertise the same set, which is harmless.
  @caps.unsafe.untrackedCaptures
  private val advertised = new AtomicReference[Option[ToolSet]](None)

  /** The tools the servers' lists offer now, each `{server}_{tool}`, free, and rerun when an
    * edge dies running it ([[Retry.Rerun]]), in the order of `clients`, then as listed; a name
    * two servers both offer is the first's. A server whose list cannot be read offers none; a
    * tool whose schema declares [[Writes.Field]] ([[Writes.declared]]) is neither offered nor
    * run, since a set advertising it would not read back.
    * Given to `advertise` as [[run]] gives it; `Left` when `advertise` refused it.
    */
  def offered(): Either[DeskError, ToolSet] = {
    val set = McpTools.current(clock, clients).map((_, tool) => McpTools.entry(tool))
    val offering = ToolSet.of(set).getOrElse(ToolSet.Empty)
    if (advertised.get().contains(offering)) Right(offering)
    else
      advertise(offering).map { _ =>
        advertised.set(Some(offering))
        offering
      }
  }

  /** `request` run as the class says, its failures each an [[Outcome.Failed]]. A refused
    * `advertise` ([[offered]]'s `Left`) does not fail it: the run proceeds with the servers'
    * current lists.
    */
  def run(route: Route, request: ToolRequest): Outcome = {
    val _ = offered()
    val tools: Vector[Tool.Offered^{clock}] =
      McpTools.current(clock, clients).flatMap { (at, tool) =>
        clients.lift(at).toVector.flatMap { client =>
          Vector(McpTools.hosted(tool).over(args => McpTools.call(client, tool, args)))
        }
      }
    Toolbox.of[caps.CapSet^{clock}](tools*) match {
      case Right(box) => Run.request(request, box)
      // current offers each name once.
      case Left(repeated) =>
        Outcome.Failed(s"two MCP tools are named ${ToolName.value(repeated.name)}; nothing ran.")
    }
  }
}

object McpTools {

  /** Each of `clients`' tools as listed now, with its client's index, each name once, the
    * first client's kept; none whose schema declares [[Writes.Field]].
    */
  private def current(clock: Clock, clients: Vector[McpClient^{clock}]): Vector[(Int, McpTool)] =
    clients
      .map(_.tools())
      .zipWithIndex
      .flatMap((listed, at) => listed.fold(_ => Vector.empty, _.tools.map((at, _))))
      .filterNot((_, tool) => Writes.declared(tool.inputSchema))
      .distinctBy(_._2.offered)

  /** `tool` as this edge describes it: its arguments the JSON object sent, unchecked; a call
    * shown as [[Hosted.advertised]] shows one.
    */
  private def hosted(tool: McpTool): Hosted[ujson.Value] =
    new Hosted[ujson.Value](
      ToolSpec(tool.offered, tool.does, Args.raw(tool.inputSchema), Retry.Rerun),
      Gate.Free,
      args => args.render().take(Hosted.ShownMax)
    )

  /** `tool` as an edge advertises it. */
  private def entry(tool: McpTool): ToolSet.Entry = hosted(tool).entry

  /** `tool` called on `client`'s server with `args`, as the model reads it. */
  private def call(client: McpClient^, tool: McpTool, args: ujson.Value): Outcome =
    args.objOpt match {
      case None => Outcome.Failed(s"${tool.name}'s arguments are not a JSON object; nothing ran.")
      case Some(obj) =>
        client.call(tool, ujson.Obj.from(obj)) match {
          case Left(error) => Outcome.Failed(s"${client.server.name} ${error.message}")
          case Right(answer) =>
            if (answer.isError) Outcome.Failed(answer.text) else Outcome.Done(answer.text)
        }
    }
}
