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
      args.objOpt.flatMap(_.get("text")).flatMap(_.strOpt) match {
        case None => Left(CommandError.Unread(name, "text is required"))
        case Some(t) if name == Echo => Right(CommandOutcome.Done(t))
        case Some(t) => Right(CommandOutcome.Refused(s"not $t"))
      }
    )
}
