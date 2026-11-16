package grit.core.edge

import grit.core.plugin.{CommandOutcome, KeepingCommand}

import EdgeContracts.*
import PluginCommandsContract.*

/** The commands contract, kept by the scripted fake scripting
  * [[PluginCommandsContract.Commands]], each reading `text`, `keep` listed as keeping.
  */
object ScriptedCommandsTests extends PluginCommandsContract {

  protected def commanding[A](body: PluginCommands^ => A): A = body(fake)

  private def fake: ScriptedCommands =
    new ScriptedCommands(
      Commands.commands.map(c =>
        ScriptedCommands.Offered.of(c.spec, reads = !c.isInstanceOf[KeepingCommand[?]])
      ),
      Set(Reader)
    )((name, args) => {
      val said = args.objOpt.flatMap(_.get("text")).flatMap(_.strOpt).getOrElse("")
      if (name == Echo) Right(CommandOutcome.Done(said))
      else Right(CommandOutcome.Refused(s"not $said"))
    })
}
