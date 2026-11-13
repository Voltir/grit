package grit.commands.routes

import grit.core.edge.CommandError
import grit.core.identity.Account
import grit.core.plugin.CommandOutcome
import grit.core.tool.ToolName
import grit.core.visibility.Integrity
import grit.edge.http.Status
import grit.mcp.wire.CallResult

/** What a command came to, as every route that runs one answers it: one table, so `/command` and
  * `/mcp` cannot disagree.
  */
private[commands] object RouteReply {

  /** The command's text 200 when done; its reason 409 when it refused; 404 for a name no
    * enabled plugin's command has; 400 for arguments it does not read; 403 for an account no
    * person is linked to; 500 when its store call failed; 503 when the database could not be
    * reached. Only a 200 kept anything.
    */
  def command(ran: Either[CommandError, CommandOutcome]): (Status, String) = ran match {
    case Right(CommandOutcome.Done(text)) => (Status.Ok, text)
    case Right(CommandOutcome.Refused(why)) => (Status.Conflict, why)
    case Left(CommandError.Unknown(name)) =>
      (Status.NotFound, s"no command is named ${ToolName.value(name)}")
    case Left(CommandError.Unread(_, reason)) => (Status.BadRequest, reason)
    case Left(CommandError.NotVouched(by)) =>
      (Status.Forbidden, s"no person is linked to ${Account.written(by)}")
    case Left(CommandError.Store(_, error)) => (Status.Failed, s"nothing was kept: $error")
    case Left(CommandError.Unavailable(cause)) =>
      (Status.Unavailable, s"the database is unavailable: $cause")
  }

  /** `answered` as an MCP `tools/call` result: 200 its text, 404 no such tool, 400 arguments the
    * command does not read, anything else its text as a failed result.
    */
  def call(answered: (Status, String)): CallResult = answered match {
    case (Status.Ok, text) => CallResult.Done(text)
    case (Status.NotFound, _) => CallResult.NoSuchTool
    case (Status.BadRequest, why) => CallResult.BadArguments(why)
    case (_, text) => CallResult.Failed(text)
  }

  /** 403, for a caller not believed at `least`, which a deployment's own command requires. */
  def below(name: ToolName, least: Integrity): (Status, String) =
    (
      Status.Forbidden,
      s"${ToolName.value(name)} is given only by a caller believed at ${Integrity.written(least)}"
    )
}
