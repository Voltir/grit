package grit.core.edge

import java.time.Instant

import grit.core.id.SessionId
import grit.core.identity.Account
import grit.core.place.Place
import grit.core.plugin.CommandOutcome
import grit.core.tool.{Args, ToolName, ToolSpec}
import grit.core.visibility.Integrity

/** [[PluginCommands]] offering `offered`, each run answered by `answer`, which is given its name
  * and arguments and may answer any error. A run is refused, in grit.dbos' `SqlPluginCommands`' order,
  * as [[CommandError.Unknown]] for a name not offered, [[CommandError.Unread]] for arguments its
  * `args` do not read, and [[CommandError.NotVouched]] for an account not in `vouched`, before
  * `answer` is asked. Every run is recorded in [[ran]], whatever it came to. Held to
  * [[PluginCommandsContract]].
  */
final class ScriptedCommands(
    offers: Vector[ScriptedCommands.Offered],
    vouched: Set[Account]
)(answer: (ToolName, ujson.Value) -> Either[CommandError, CommandOutcome])
    extends PluginCommands {

  // Replaced whole by `run`, under this fake's lock, and read whole by `ran`; nothing else holds it.
  @caps.unsafe.untrackedCaptures
  private var runs: Vector[ScriptedCommands.Ran] = Vector.empty

  /** Every run so far, first first. */
  def ran: Vector[ScriptedCommands.Ran] = synchronized(runs)

  val offered: Vector[PluginCommands.Listed] = offers.map(_.listed)

  def run(
      by: Account,
      room: Place,
      believed: Integrity,
      session: Option[SessionId],
      name: ToolName,
      args: ujson.Value,
      at: Instant
  ): Either[CommandError, CommandOutcome] = {
    synchronized {
      runs = runs :+ ScriptedCommands.Ran(by, room, believed, session, name, args, at)
    }
    offers.find(_.listed.schema.name == ToolName.value(name)) match {
      case None => Left(CommandError.Unknown(name))
      case Some(o) =>
        o.args.read(args) match {
          case Left(e) => Left(CommandError.Unread(name, e.message))
          case Right(_) =>
            if (!vouched.contains(by)) Left(CommandError.NotVouched(by)) else answer(name, args)
        }
    }
  }
}

object ScriptedCommands {

  /** A command offered as `listed`, its arguments read by `args`. */
  final case class Offered(listed: PluginCommands.Listed, args: Args[?])

  object Offered {

    /** `spec`, listed as a command's tool list shows it, reading only when `reads`. */
    def of(spec: ToolSpec[?], reads: Boolean): Offered =
      Offered(PluginCommands.Listed(spec.schema(strict = false), reads), spec.args)
  }

  /** One run, as it was asked. */
  final case class Ran(
      by: Account,
      room: Place,
      believed: Integrity,
      session: Option[SessionId],
      name: ToolName,
      args: ujson.Value,
      at: Instant
  )
}
