package grit.mcp.edge

import java.util.concurrent.atomic.AtomicReference

import grit.core.edge.{DeskError, Route, ToolRequest}
import grit.core.place.Directory
import grit.core.tool.{
  Args,
  Believed,
  Gate,
  Hosted,
  Outcome,
  Retry,
  Tool,
  ToolName,
  ToolSet,
  ToolSpec,
  Toolbox
}
import grit.core.visibility.Integrity
import grit.edge.desk.{Run, Tools}
import grit.mcp.client.McpClient
import grit.mcp.wire.{McpError, McpTool}

/** The requests an edge is sent, run by calling `clients`' servers, wherever they were routed:
  * a request's tool is found by name in the servers' lists as they stand ([[McpClient.tools]],
  * listed again when stale), and run as [[Run.request]] runs a tool, its arguments the JSON
  * object sent. Its answer is [[Outcome.Done]], its items, parts and facts the call's answer's
  * ([[McpClient.call]]), or [[Outcome.Failed]] with the tool's text
  * when it marks it `isError`, or naming the server and the [[grit.mcp.wire.McpError]] when
  * the exchange fails; the run's `said` then hears the cause ([[grit.mcp.wire.McpError.logged]]),
  * its tool named. Before each run, the tools are told to `advertise` when they differ
  * from those it last took (the first time included); one it refused is told again next run.
  * A call of a server that reads the asking conversation's directory
  * ([[grit.mcp.client.McpServer.directoryArgument]]) is sent with the one `asking` tells for its
  * request, or Failed, unsent, when `asking` cannot tell. `reach` is what `clients` capture, and
  * nothing more reaches them: the clock their lists are fresh by, or a client itself.
  */
// `reach` is a parameter, rather than a capture-set parameter over `clients`, so the class's
// captures are in its signature (docs/capture-checking.md, "A capset parameter a class's body
// alone uses").
final class McpTools(
    reach: AnyRef^,
    clients: Vector[McpClient^{reach}],
    advertise: ToolSet => Either[DeskError, Unit],
    asking: ToolRequest => Either[String, Option[Directory]] = _ => Right(None)
) extends Tools {

  // The set `advertise` last took. Replaced whole; two runs that both find it changed both
  // advertise the same set, which is harmless.
  @caps.unsafe.untrackedCaptures
  private val advertised = new AtomicReference[Option[ToolSet]](None)

  /** The tools the servers' lists offer now, each `{server}_{tool}`, free, and rerun when an
    * edge dies running it ([[Retry.Rerun]]), in the order of `clients`, then as listed; a name
    * two servers both offer is the first's. A server whose list cannot be read offers none; a
    * tool whose schema declares [[grit.core.tool.Writes.Field]] is neither offered nor run,
    * since a set advertising it would not read back: its listing skips it
    * ([[grit.mcp.wire.Skipped.DeclaresField]]).
    * Given to `advertise` as [[run]] gives it; `Left` when `advertise` refused it.
    */
  def offered(): Either[DeskError, ToolSet] = {
    val set = McpTools.current(reach, clients).map((_, tool) => McpTools.entry(tool))
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
  def run(route: Route, request: ToolRequest, said: String => Unit): Outcome = {
    val _ = offered()
    val tools: Vector[Tool.Offered^{reach, said, asking}] =
      McpTools.current(reach, clients).flatMap { (at, tool) =>
        clients.lift(at).toVector.flatMap { client =>
          Vector(
            McpTools
              .hosted(tool)
              .over(args => McpTools.call(client, tool, args, said, () => asking(request)))
          )
        }
      }
    Toolbox.of[caps.CapSet^{reach, said, asking}](tools*) match {
      case Right(box) => Run.request(request, box)
      // current offers each name once.
      case Left(repeated) =>
        Outcome.Failed(s"two MCP tools are named ${ToolName.value(repeated.name)}; nothing ran.")
    }
  }
}

object McpTools {

  /** Each of `clients`' tools as listed now, with its client's index, each name once, the
    * first client's kept.
    */
  private def current(reach: AnyRef^, clients: Vector[McpClient^{reach}]): Vector[(Int, McpTool)] =
    clients
      .map(_.tools())
      .zipWithIndex
      .flatMap((listed, at) => listed.fold(_ => Vector.empty, _.tools.map((at, _))))
      .distinctBy(_._2.offered)

  /** `tool` as this edge describes it: its arguments the JSON object sent, unchecked; a call
    * shown as [[Hosted.advertised]] shows one.
    */
  private def hosted(tool: McpTool): Hosted[ujson.Value] =
    new Hosted[ujson.Value](
      ToolSpec(tool.offered, tool.does, Args.raw(tool.inputSchema), Retry.Rerun),
      Gate.Free,
      args => args.render().take(Hosted.ShownMax),
      // A call at its service is believed at the service's output; this holds where none is.
      Believed.At(Integrity.Untrusted)
    )

  /** `tool` as an edge advertises it. */
  private def entry(tool: McpTool): ToolSet.Entry = hosted(tool).entry

  /** `tool` called on `client`'s server with `args`, for the conversation working where `asking`
    * tells, read only when the server reads it
    * ([[grit.mcp.client.McpServer.directoryArgument]]); as the model
    * reads it, `said` told why when the exchange failed.
    */
  private def call(
      client: McpClient^,
      tool: McpTool,
      args: ujson.Value,
      said: String => Unit,
      asking: () => Either[String, Option[Directory]]
  ): Outcome = {
    val named = s"${ToolName.value(tool.offered)}: ${client.server.name}"
    args.objOpt match {
      case None => Outcome.Failed(s"${tool.name}'s arguments are not a JSON object; nothing ran.")
      case Some(obj) =>
        val where =
          if (client.server.directoryArgument.isEmpty) Right(None)
          else asking()
        where match {
          case Left(why) =>
            val error =
              McpError.Unsent(s"the directory of the conversation asking could not be told: $why")
            error.logged.foreach(w => said(s"$named $w"))
            Outcome.Failed(s"${client.server.name} ${error.message}")
          case Right(at) =>
            client.call(tool, ujson.Obj.from(obj), at) match {
              case Left(error) =>
                error.logged.foreach(why => said(s"$named $why"))
                Outcome.Failed(s"${client.server.name} ${error.message}")
              case Right(answer) =>
                if (answer.isError) Outcome.Failed(answer.text)
                else
                  Outcome.Done(
                    answer.text,
                    items = answer.items,
                    parts = answer.parts,
                    facts = answer.facts
                  )
            }
        }
    }
  }
}
