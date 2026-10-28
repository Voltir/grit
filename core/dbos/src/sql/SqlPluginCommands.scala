package grit.dbos.sql

import java.time.Instant
import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.document.{DocumentKeeper, DocumentTerms}
import grit.core.edge.{CommandError, PluginCommands}
import grit.core.identity.{Account, Principal}
import grit.core.need.PluginReads
import grit.core.place.Place
import grit.core.plugin.{CommandOutcome, KeepingCommand, Plugin, ReadingCommand}
import grit.core.provider.ToolSchema
import grit.core.store.{StoreError, Tx}
import grit.core.tool.ToolName
import grit.core.visibility.{Integrity, Subject}

/** [[PluginCommands]] over `dataSource`, of `plugins`' commands: each run is one transaction on
  * one connection, its asker resolved under [[Subject.Public]] (as [[SqlAdministration]] does)
  * and then the command run under [[Subject.Person]], both opened through `opener`. A reading
  * command's connection is read-only and always rolled back; a keeping command's is committed
  * only when it answers [[CommandOutcome.Done]]. A keeping command of a plugin with no documents,
  * which a deployment refuses, is not offered.
  */
private[dbos] final class SqlPluginCommands(
    dataSource: DataSource,
    opener: Opener,
    plugins: Vector[Plugin]
) extends PluginCommands {

  /** A command as this runs it: a reading one over its plugin's reads, or a keeping one under its
    * plugin's terms.
    */
  private enum Runnable {
    case Reading(plugin: Plugin, command: ReadingCommand[?])
    case Keeping(plugin: Plugin, terms: DocumentTerms, command: KeepingCommand[?])
  }

  private val documents = new SqlDocuments(new SqlTombstones)

  private val runnable: Vector[(ToolName, Runnable)] =
    plugins.flatMap(p =>
      p.commands.flatMap {
        case r: ReadingCommand[?] => Vector(r.spec.name -> Runnable.Reading(p, r))
        case k: KeepingCommand[?] =>
          p.documents.toVector.map(d => k.spec.name -> Runnable.Keeping(p, d.terms, k))
      }
    )

  def offered: Vector[ToolSchema] = runnable.map {
    case (_, Runnable.Reading(_, c)) => c.spec.schema(strict = false)
    case (_, Runnable.Keeping(_, _, c)) => c.spec.schema(strict = false)
  }

  def run(
      by: Account,
      room: Place,
      believed: Integrity,
      name: ToolName,
      args: ujson.Value,
      at: Instant
  ): Either[CommandError, CommandOutcome] =
    runnable.find(_._1 == name).map(_._2) match {
      case None => Left(CommandError.Unknown(name))
      case Some(Runnable.Reading(plugin, command)) =>
        reading(command, plugin, by, room, name, args, at)
      case Some(Runnable.Keeping(plugin, terms, command)) =>
        keeping(command, plugin, terms, by, room, believed, name, args, at)
    }

  private def reading[A <: caps.Pure](
      command: ReadingCommand[A],
      plugin: Plugin,
      by: Account,
      room: Place,
      name: ToolName,
      args: ujson.Value,
      at: Instant
  ): Either[CommandError, CommandOutcome] =
    command.spec.args.read(args) match {
      case Left(e) => Left(CommandError.Unread(name, e.message))
      case Right(a) =>
        val own = PluginReads(new SqlPluginDocs(plugin.name), documents.shelf(plugin.name))
        transaction(by, room, name, keeps = false)((person, tx) =>
          command.run(a, person, own, at)(using tx)
        )
    }

  private def keeping[A <: caps.Pure](
      command: KeepingCommand[A],
      plugin: Plugin,
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
          // As an inbound entry is believed (SqlEntryStore): no more than the room's cap.
          val most = Tx.visibility(tx).capOf(room).fold(believed)(believed.meet)
          val own = DocumentKeeper.believedAtMost(documents.keeper(plugin.name, terms), most)
          command.run(a, person, own, at)(using tx)
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
        val asked = opener
          .open(Subject.Public, conn)
          .flatMap(tx => SqlIdentities.person(by)(using tx))
        val ran: Either[CommandError, CommandOutcome] = asked match {
          case Left(e) => Left(CommandError.Unavailable(e.toString))
          case Right(Some(person: Principal.Person)) =>
            opener.open(Subject.Person(person, room), conn) match {
              case Left(e) => Left(CommandError.Unavailable(e.toString))
              case Right(tx) =>
                try body(person, tx).left.map(CommandError.Store(name, _))
                catch {
                  case NonFatal(e) =>
                    Left(CommandError.Store(name, SqlEntryStore.databaseError(e)))
                }
            }
          case Right(_) => Left(CommandError.NotVouched(by))
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
