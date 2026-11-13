package grit.core.edge

import grit.core.plugin.CommandOutcome

import EdgeContracts.*
import PluginCommandsContract.*

/** The commands contract, kept by the scripted fake scripting
  * [[PluginCommandsContract.Commands]]: `echo` and `refuse`, each reading `text`.
  */
object ScriptedCommandsTests extends PluginCommandsContract {

  protected def commanding[A](body: PluginCommands^ => A): A = body(fake)

  private def fake: ScriptedCommands =
    new ScriptedCommands(
      Commands.commands.map(c => PluginCommands.Listed(c.spec.schema(false), reads = true)),
      Set(Reader)
    )((name, args) =>
      text.read(args) match {
        case Left(e) => Left(CommandError.Unread(name, e.message))
        case Right(Text(t)) if name == Echo => Right(CommandOutcome.Done(t))
        case Right(Text(t)) => Right(CommandOutcome.Refused(s"not $t"))
      }
    )
}
