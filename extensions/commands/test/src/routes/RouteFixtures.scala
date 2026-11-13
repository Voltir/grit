package grit.commands.routes

import java.time.Instant

import grit.commands.caller.{Caller, Callers}
import grit.commands.worker.WorkerName
import grit.core.edge.{CommandError, EdgeContracts, EdgeWatch, PluginCommands, ScriptedCommands}
import grit.core.plugin.CommandOutcome
import grit.core.provider.ToolSchema
import grit.core.tool.ToolName
import grit.core.visibility.Integrity
import grit.edge.http.{Request, Verb}
import grit.mcp.wire.ServerInfo

/** What the routes' suites share: the owner, as the contracts' reader, and requests to them. */
object RouteFixtures {

  def got[A](e: Either[String, A]): A =
    e.fold(why => throw new java.lang.AssertionError(why), a => a)

  val Worker: WorkerName = got(WorkerName.of("desktop"))

  /** The contracts' reader, believed at member, on [[Worker]]. */
  val Owner: Caller = Caller(EdgeContracts.Reader, Integrity.Member, Worker)

  val Now: Instant = EdgeContracts.At

  /** A request as the listener reads one, from this machine. */
  def request(
      verb: Verb,
      target: String,
      body: String = "",
      headers: Vector[(String, String)] = Vector("Host" -> "localhost:7317")
  ): Request = got(Request.of(verb, target, headers, body))

  /** A command offered by its name, which reads `text`. */
  def listed(name: String, reads: Boolean): PluginCommands.Listed =
    PluginCommands.Listed(
      ToolSchema(
        name,
        s"Runs $name.",
        ujson.Obj(
          "type" -> "object",
          "properties" -> ujson.Obj("text" -> ujson.Obj("type" -> "string")),
          "required" -> ujson.Arr("text")
        )
      ),
      reads
    )

  /** `offered`, run for [[Owner]] alone, each answering `answer`. */
  def scripted(offered: Vector[PluginCommands.Listed])(
      answer: (ToolName, ujson.Value) -> Either[CommandError, CommandOutcome]
  ): ScriptedCommands =
    new ScriptedCommands(offered, Set(Owner.account))(answer)

  /** A watch no test of a command's route reaches. */
  def Unwatched: EdgeWatch = EdgeWatch.Refusing

  val Server: ServerInfo = ServerInfo("commands", "1", None)

  /** Requests with no credential are [[Owner]]'s. */
  val Owners: Callers = Callers.owner(Owner)
}
