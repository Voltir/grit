package grit.commands.declare

import grit.core.edge.CommandError
import grit.core.plugin.CommandOutcome
import grit.core.tool.ToolName
import grit.core.visibility.Integrity

/** Runs an enabled plugin's command, by name with its arguments, as the caller of the request it
  * was made for, in the edge's room: an own command's way to the plugins' commands.
  */
type Ran = (ToolName, ujson.Value) => Either[CommandError, CommandOutcome]

/** A deployment's own command at the commands edge, given by `name`: a caller not believed at
  * `least` is refused, nothing run; any other gets `run` of the request's arguments, lent `ran`
  * for that request alone.
  */
final case class OwnCommand private (
    name: ToolName,
    least: Integrity,
    guards: Boolean,
    run: (Ran, ujson.Obj) -> Either[CommandError, CommandOutcome]
) extends caps.Pure

object OwnCommand {

  /** In place of the offered plugin command `name`, at `/command` and `/mcp` alike, listed at
    * `/mcp` with that command's own schema; `ran` runs it, or any other, as it would have run.
    * The edge refuses to open when no plugin offers `name`.
    */
  def guarding(
      name: ToolName,
      least: Integrity,
      run: (Ran, ujson.Obj) -> Either[CommandError, CommandOutcome]
  ): OwnCommand = OwnCommand(name, least, guards = true, run)

  /** A command no plugin offers, at `/command` alone. The edge refuses to open when a plugin
    * offers `name`, or another own command is named so.
    */
  def added(
      name: ToolName,
      least: Integrity,
      run: (Ran, ujson.Obj) -> Either[CommandError, CommandOutcome]
  ): OwnCommand = OwnCommand(name, least, guards = false, run)
}
