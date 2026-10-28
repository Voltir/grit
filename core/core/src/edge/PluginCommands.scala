package grit.core.edge

import java.time.Instant

import grit.core.identity.Account
import grit.core.place.Place
import grit.core.plugin.Answered
import grit.core.provider.ToolSchema
import grit.core.store.StoreError
import grit.core.tool.ToolName
import grit.core.visibility.Integrity

/** The enabled plugins' commands, as an edge runs them for the people it serves (ADR 0048). */
trait PluginCommands extends caps.SharedCapability {

  /** Every enabled plugin's commands, as a tool list shows them. */
  def offered: Vector[ToolSchema]

  /** Runs the command `name` with `args`, given through `by` in `room` at `at`, in one transaction
    * opened for the person `by` is linked to then, in `room`
    * ([[grit.core.visibility.Subject.Person]]); what it keeps is kept at `room`'s label joined
    * with the transaction's floor and believed at no more than `believed` (the integrity the edge
    * gives what `by` says there) met with `room`'s declared cap. [[CommandError.Unknown]] for a
    * name no enabled plugin's command has; [[CommandError.Unread]] with the reason, for arguments
    * its spec does not read; [[CommandError.NotVouched]] for an account no person is linked to;
    * [[CommandError.Store]] when the command's own store call failed (its plugin's bound refusing
    * a write, [[StoreError.Full]]; a reading command's write); [[CommandError.Unavailable]] when
    * the database could not open or commit the transaction. Nothing is kept on a `Left`.
    */
  def run(
      by: Account,
      room: Place,
      believed: Integrity,
      name: ToolName,
      args: ujson.Value,
      at: Instant
  ): Either[CommandError, Answered]
}

object PluginCommands {

  /** Offers no command, and answers every name [[CommandError.Unknown]]. */
  def None: PluginCommands = new PluginCommands {
    def offered: Vector[ToolSchema] = Vector.empty
    def run(
        by: Account,
        room: Place,
        believed: Integrity,
        name: ToolName,
        args: ujson.Value,
        at: Instant
    ): Either[CommandError, Answered] = Left(CommandError.Unknown(name))
  }
}

/** Why [[PluginCommands.run]] ran no command, or kept nothing of one. */
enum CommandError {

  /** No enabled plugin has a command named `name`. */
  case Unknown(name: ToolName)

  /** `name`'s spec does not read the arguments given, for `reason`. */
  case Unread(name: ToolName, reason: String)

  /** No person is linked to the account `by`. */
  case NotVouched(by: Account)

  /** The command's own store call failed with `error`; nothing was kept. */
  case Store(name: ToolName, error: StoreError)

  /** The database could not open or commit the command's transaction, for `cause`. */
  case Unavailable(cause: String)
}
