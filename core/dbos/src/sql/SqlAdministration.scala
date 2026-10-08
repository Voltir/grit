package grit.dbos.sql

import java.sql.PreparedStatement
import java.time.{Instant, ZoneOffset}

import scala.util.Using

import grit.core.admin.{Administration, Answer, Change, ChangeJson, Command}
import grit.core.identity.{Account, Principal}
import grit.core.place.Place
import grit.core.store.{Jot, StoreError, Tx}
import grit.core.visibility.{GroupName, Subject}

/** [[Administration]] over Postgres: each command one transaction written through `jot` (so
  * nothing is kept unless all of it is), its changes kept in `grit.rooms` (a room never heard
  * gets its place first, [[SqlPlaces.id]]) and `grit.group_members`, each change made with one
  * `grit.visibility_changes` row. Settings carry no label, so it opens for
  * [[Subject.Public]]. A change's answer is read under the labels in force after it, read
  * again on the same transaction through `opener`.
  */
private[dbos] final class SqlAdministration(jot: Jot, opener: Opener) extends Administration {
  import SqlEntryStore.attempt

  def run(by: Account, room: Place, command: Command, at: Instant): Either[StoreError, Answer] =
    jot.write(Subject.Public) { (tx: Tx^) ?=>
      val named = command match {
        case Command.Clearance(Some(of)) => SqlIdentities.person(of)
        case _ => Right(None)
      }
      SqlIdentities.person(by).flatMap { asker =>
        named.flatMap { named =>
          Administration.decide(asker, room, command, named) match {
            case Left(answer) => Right(answer)
            case Right(allowed) =>
              val change = allowed.change
              for {
                changed <- keep(change)
                _ <- if (changed) audit(by, at, change) else Right(())
                person <- change match {
                  case Change.Clear(p, _) => SqlIdentities.person(p)
                  case Change.Remove(p, _) => SqlIdentities.person(p)
                  case Change.Relabel(_, _, _) | Change.Quiet(_, _) => Right(None)
                }
                answer <- answered(allowed, person)
              } yield answer
          }
        }
      }
    }

  /** `kept`'s answer, under the labels in force read again on `tx`'s own connection, which
    * sees what this transaction has written.
    */
  private def answered(kept: grit.core.admin.Authority.Allowed, person: Option[Principal])(using
      tx: Tx^
  ): Either[StoreError, Answer] =
    opener.at(Tx.clearance(tx), Tx.connection(tx)).map { after =>
      Administration.answer(kept, person)(using after)
    }

  /** Keeps `change`; whether anything recorded changed (a repeat changes nothing). */
  private def keep(change: Change)(using tx: Tx^): Either[StoreError, Boolean] =
    change match {
      case Change.Relabel(room, _, Change.To.Set(label)) =>
        for {
          place <- SqlPlaces.id(room)
          id <- SqlLabels.intern(label)
          n <- update(
            """INSERT INTO grit.rooms (place_id, label_id) VALUES (?::uuid, ?)
              |ON CONFLICT (place_id) DO UPDATE SET label_id = EXCLUDED.label_id
              | WHERE grit.rooms.label_id IS DISTINCT FROM EXCLUDED.label_id""".stripMargin
          ) { ps =>
            ps.setString(1, place)
            ps.setInt(2, id)
          }
        } yield n > 0
      case Change.Relabel(room, _, Change.To.Default(_)) =>
        atRoom(room, "label_id = NULL", "r.label_id IS NOT NULL")
      case Change.Quiet(room, true) =>
        SqlPlaces.id(room).flatMap { place =>
          update(
            """INSERT INTO grit.rooms (place_id, quiet) VALUES (?::uuid, true)
              |ON CONFLICT (place_id) DO UPDATE SET quiet = true WHERE NOT grit.rooms.quiet""".stripMargin
          )(_.setString(1, place)).map(_ > 0)
        }
      case Change.Quiet(room, false) => atRoom(room, "quiet = false", "r.quiet")
      case Change.Clear(person, c) =>
        SqlIdentities.enroll(Set(person)).flatMap { _ =>
          update(
            """INSERT INTO grit.group_members (group_name, account) VALUES (?, ?)
              |ON CONFLICT DO NOTHING""".stripMargin
          )(member(_, GroupName.own(c), person)).map(_ > 0)
        }
      case Change.Remove(person, c) =>
        update("DELETE FROM grit.group_members WHERE group_name = ? AND account = ?")(
          member(_, GroupName.own(c), person)
        ).map(_ > 0)
    }

  /** Sets `set` on `room`'s `grit.rooms` row where `unless` holds of it (aliased `r`); whether
    * it did. Nothing for a room with no row: nothing is recorded of it to undo.
    */
  private def atRoom(room: Place, set: String, unless: String)(using
      Tx^
  ): Either[StoreError, Boolean] =
    update(
      s"""UPDATE grit.rooms r SET $set FROM grit.places p
         | WHERE p.id = r.place_id AND p.path = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))
         |   AND $unless""".stripMargin
    )(_.setString(1, SqlPlaces.path(room))).map(_ > 0)

  /** One `grit.visibility_changes` row: `change`, made by `by` at `at`. */
  private def audit(by: Account, at: Instant, change: Change)(using
      Tx^
  ): Either[StoreError, Unit] =
    update("INSERT INTO grit.visibility_changes (at, by, change) VALUES (?, ?, ?::jsonb)") { ps =>
      ps.setObject(1, at.atOffset(ZoneOffset.UTC))
      ps.setString(2, SqlIdentities.written(by))
      ps.setString(3, ChangeJson.write(change).render())
    }.map(_ => ())

  private def member(ps: PreparedStatement, group: GroupName, account: Account): Unit = {
    ps.setString(1, GroupName.value(group))
    ps.setString(2, SqlIdentities.written(account))
  }

  /** `sql` run with `bind`'s parameters; the rows it changed. */
  private def update(sql: String)(bind: PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Int] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        bind(ps)
        ps.executeUpdate()
      }
    }
  }
}
