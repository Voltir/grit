package grit.commands.routes

import java.time.Instant

import grit.commands.caller.{Caller, Callers}
import grit.commands.declare.{HoldingWatch, OwnCommand}
import grit.commands.machine.MachineName
import grit.core.clock.SetClock
import grit.core.edge.{
  Adapter,
  CommandError,
  EdgeContracts,
  EdgeSessions,
  EdgeWatch,
  PluginCommands,
  ScriptedCommands,
  ScriptedSessions
}
import grit.core.either.Got.got
import grit.core.id.Sha256
import grit.core.plugin.CommandOutcome
import grit.core.tool.{Args, Field, ToolName, ToolSpec}
import grit.core.visibility.Integrity
import grit.edge.http.{Hosts, Request, Verb}
import grit.mcp.wire.ServerInfo

/** What the routes' suites share: the owner, as the contracts' reader, and requests to them. */
object RouteFixtures {

  val Machine: MachineName = got(MachineName.of("desktop"))

  /** The contracts' reader, believed at member, on [[Machine]]. */
  val Owner: Caller = Caller(EdgeContracts.Reader, Integrity.Member, Machine)

  val Now: Instant = EdgeContracts.At

  /** A request as the listener reads one, from this machine. */
  def request(
      verb: Verb,
      target: String,
      body: String = "",
      headers: Vector[(String, String)] = Vector("Host" -> "localhost:7317")
  ): Request = got(Request.of(verb, target, headers, body))

  /** What every scripted command reads: one `text`. */
  val Said: Args[String] = Args.of((text = Field.text("What to say."))).map(_.text)

  /** A command offered by its name, which reads [[Said]]. */
  def listed(name: String, reads: Boolean): ScriptedCommands.Offered =
    ScriptedCommands.Offered.of(ToolSpec(got(ToolName.of(name)), s"Runs $name.", Said), reads)

  /** `offered`, run for [[Owner]] alone, each answering `answer`. */
  def scripted(offered: Vector[ScriptedCommands.Offered])(
      answer: (ToolName, ujson.Value) -> Either[CommandError, CommandOutcome]
  ): ScriptedCommands =
    new ScriptedCommands(offered, Set(Owner.account))(answer)

  /** A watch no test of a command's route reaches. */
  def Unwatched: EdgeWatch = EdgeWatch.Refusing

  /** Sessions no test of a command's route reaches. */
  def Unrecorded: EdgeSessions = EdgeSessions.Refusing

  val Server: ServerInfo = ServerInfo("commands", "1", None)

  /** Requests with no credential are [[Owner]]'s. */
  val Owners: Callers = Callers.owner(Owner)

  /** A second machine. */
  val Laptop: MachineName = got(MachineName.of("laptop"))

  /** [[Owner]]'s account at their own command line, believed at authority, on [[Machine]]. */
  val Own: Caller = Owner.copy(believed = Integrity.Authority)

  /** [[Owner]]'s account as their agents on [[Laptop]] work, believed at member. */
  val Away: Caller = Owner.copy(machine = Laptop)

  /** The credentials' texts: [[Own]]'s, [[Owner]]'s (the sessions the owner starts on
    * [[Machine]]) and [[Away]]'s.
    */
  val OwnText: String = "own-0123456789abcdefghijklmnopqrstuvwxyz"
  val AgentText: String = "agent-0123456789abcdefghijklmnopqrstuvwxyz"
  val AwayText: String = "away-0123456789abcdefghijklmnopqrstuvwxyz"

  /** One person's three credentials: [[OwnText]] is [[Own]], [[AgentText]] [[Owner]] and
    * [[AwayText]] [[Away]].
    */
  val Credentialed: Callers = got(
    Callers.of(
      Callers.Token(Sha256.text(OwnText), Own),
      Callers.Token(Sha256.text(AgentText), Owner),
      Callers.Token(Sha256.text(AwayText), Away)
    )
  )

  /** A request's headers from this machine, bearing `text`. */
  def bearing(text: String): Vector[(String, String)] =
    Vector("Host" -> "localhost:7317", "Authorization" -> s"Bearer $text")

  /** The routes over `commands`, `watch` and the sessions `script` holds for [[Owner]] (or for no
    * one, unless `vouched`), joined by `adapters`, `callers` on loopback, at [[Now]] in
    * [[EdgeContracts.Room]], with `own` commands and `holding`'s watch.
    */
  def routed(
      commands: ScriptedCommands = scripted(Vector.empty)(Unrun),
      script: ScriptedSessions.Script = new ScriptedSessions.Script,
      vouched: Boolean = true,
      adapters: Vector[Adapter] = Vector.empty,
      callers: Callers = Owners,
      own: Vector[OwnCommand] = Vector.empty,
      watch: EdgeWatch = Unwatched,
      holding: Option[HoldingWatch] = None
  ): Routes^ =
    got(
      Routes.of(
        commands,
        watch,
        new ScriptedSessions(if (vouched) Set(Owner.account) else Set.empty, script),
        new SetClock(Now),
        EdgeContracts.Room,
        Hosts.loopback,
        callers,
        own,
        holding,
        adapters,
        Server
      )
    )

  /** What a command no test of a session's route runs answers. */
  val Unrun: (ToolName, ujson.Value) -> Either[CommandError, CommandOutcome] =
    (_, _) => Left(CommandError.Unavailable("no command runs here"))
}
