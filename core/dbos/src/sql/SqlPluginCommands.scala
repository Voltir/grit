package grit.dbos.sql

import java.time.Instant
import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.document.DocumentTerms
import grit.core.edge.{CommandError, PluginCommands}
import grit.core.id.PluginName
import grit.core.identity.{Account, Principal}
import grit.core.need.PluginReads
import grit.core.place.Place
import grit.core.plugin.{
  CommandBy,
  CommandOutcome,
  Commanded,
  KeepingCommand,
  Plugin,
  ReadingCommand
}
import grit.core.provider.ToolSchema
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.Integrity

/** [[PluginCommands]] over `dataSource`, of `plugins`' commands: each run is one transaction on
  * one connection, its asker resolved under [[Subject.Public]] (as [[SqlAdministration]] does)
  * and then the command run under [[Subject.Person]], both opened through `opener`. A reading
  * command's connection is read-only and always rolled back; a keeping command, handed its
  * plugin's keeper and claims ([[SqlClaims]]), is committed only when it answers
  * [[CommandOutcome.Done]]. A keeping command of a plugin with no documents,
  * which a deployment refuses, is not offered.
  */
private[dbos] final class SqlPluginCommands(
    dataSource: DataSource,
    opener: Opener,
    plugins: Vector[Plugin]
) extends PluginCommands {

  private val documents = new SqlDocuments(new SqlTombstones)

  private val commanded: Vector[Commanded] = Commanded.of(plugins)

  def offered: Vector[ToolSchema] = commanded.map(_.command.spec.schema(strict = false))

  def run(
      by: Account,
      room: Place,
      believed: Integrity,
      name: ToolName,
      args: ujson.Value,
      at: Instant
  ): Either[CommandError, CommandOutcome] =
    commanded.find(_.command.spec.name == name) match {
      case None => Left(CommandError.Unknown(name))
      case Some(Commanded.Reading(plugin, command)) =>
        reading(command, plugin, by, room, name, args, at)
      case Some(Commanded.Keeping(plugin, terms, command)) =>
        keeping(command, plugin, terms, by, room, believed, name, args, at)
    }

  private def reading[A <: caps.Pure](
      command: ReadingCommand[A],
      plugin: PluginName,
      by: Account,
      room: Place,
      name: ToolName,
      args: ujson.Value,
      at: Instant
  ): Either[CommandError, CommandOutcome] =
    command.spec.args.read(args) match {
      case Left(e) => Left(CommandError.Unread(name, e.message))
      case Right(a) =>
        val own = PluginReads(new SqlPluginDocs(plugin), documents.shelf(plugin))
        transaction(by, room, name, keeps = false)((person, tx) =>
          command.run(a, CommandBy.Direct(person), own, at)(using tx)
        )
    }

  private def keeping[A <: caps.Pure](
      command: KeepingCommand[A],
      plugin: PluginName,
      terms: DocumentTerms,
      by: Account,
      room: Place,
      believed: Integrity,
      name: ToolName,
      args: ujson.Value,
      at: Instant
  ): Either[CommandError, CommandOutcome] =
    command.spec.args.read(args) match {
      case Left(e) => Left(CommandError.Unread(name, e.message))
      case Right(a) =>
        transaction(by, room, name, keeps = true) { (person, tx) =>
          val keeper = documents.keeper(plugin, terms)
          val asker = CommandBy.Direct(person)
          val claims = new SqlClaims(plugin)
          KeepingCommand.run(command, a, asker, keeper, claims, room, believed, at)(using tx)
        }
    }

  /** `body` run for the person `by` is linked to, in `room`, on one connection: read-only and
    * rolled back unless `keeps`, and then committed only on [[CommandOutcome.Done]]. A store call of
    * `body` failing, by a `Left` or a throw, is [[CommandError.Store]]; the database failing
    * otherwise, [[CommandError.Unavailable]].
    */
  private def transaction(by: Account, room: Place, name: ToolName, keeps: Boolean)(
      body: (Principal.Person, Tx^) => Either[StoreError, CommandOutcome]
  ): Either[CommandError, CommandOutcome] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        conn.setReadOnly(!keeps)
        val ran: Either[CommandError, CommandOutcome] =
          SqlAsking.as(conn, opener, by, room)(
            CommandError.NotVouched(_),
            CommandError.Unavailable(_)
          ) { (person, tx) =>
            try body(person, tx).left.map(CommandError.Store(name, _))
            catch {
              case NonFatal(e) => Left(CommandError.Store(name, SqlEntryStore.databaseError(e)))
            }
          }
        ran match {
          case Right(CommandOutcome.Done(_)) if keeps => conn.commit()
          case _ => conn.rollback()
        }
        ran
      }
    } catch {
      case NonFatal(e) => Left(CommandError.Unavailable(SqlEntryStore.databaseError(e).cause))
    }
}
