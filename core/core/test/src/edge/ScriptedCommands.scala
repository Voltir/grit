package grit.core.edge

import java.time.Instant

import grit.core.identity.Account
import grit.core.place.Place
import grit.core.plugin.CommandOutcome
import grit.core.tool.ToolName
import grit.core.visibility.Integrity

/** [[PluginCommands]] offering `offered`, each run answered by `answer`, which is given its name
  * and arguments and may answer any error; a name not offered is [[CommandError.Unknown]] and an
  * account not in `vouched` [[CommandError.NotVouched]] before `answer` is asked. Every run is
  * recorded in [[ran]], whatever it came to. Held to [[PluginCommandsContract]].
  */
final class ScriptedCommands(
    val offered: Vector[PluginCommands.Listed],
    vouched: Set[Account]
)(answer: (ToolName, ujson.Value) -> Either[CommandError, CommandOutcome])
    extends PluginCommands {

  // Replaced whole by `run`, under this fake's lock, and read whole by `ran`; nothing else holds it.
  @caps.unsafe.untrackedCaptures
  private var runs: Vector[ScriptedCommands.Ran] = Vector.empty

  /** Every run so far, first first. */
  def ran: Vector[ScriptedCommands.Ran] = synchronized(runs)

  def run(
      by: Account,
      room: Place,
      believed: Integrity,
      name: ToolName,
      args: ujson.Value,
      at: Instant
  ): Either[CommandError, CommandOutcome] = {
    synchronized { runs = runs :+ ScriptedCommands.Ran(by, room, believed, name, args, at) }
    if (!offered.exists(_.schema.name == ToolName.value(name))) Left(CommandError.Unknown(name))
    else if (!vouched.contains(by)) Left(CommandError.NotVouched(by))
    else answer(name, args)
  }
}

object ScriptedCommands {

  /** One run, as it was asked. */
  final case class Ran(
      by: Account,
      room: Place,
      believed: Integrity,
      name: ToolName,
      args: ujson.Value,
      at: Instant
  )
}
