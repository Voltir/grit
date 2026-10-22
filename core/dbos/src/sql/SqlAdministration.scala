package grit.dbos.sql

import java.sql.PreparedStatement
import java.time.{Instant, ZoneOffset}

import grit.core.admin.{Administration, Answer, Change, ChangeJson, Command, Refusal}
import grit.core.id.{ConversationId, TurnRef, TurnSeq, WorkflowId}
import grit.core.identity.{Account, Principal}
import grit.core.place.Place
import grit.core.store.{Jot, Origin, StoreError, Tx}
import grit.core.visibility.{GroupName, Subject}

/** [[Administration]] over Postgres: each command one transaction written through `jot` (so
  * nothing is kept unless all of it is), its changes kept in `grit.rooms` (a room never heard
  * gets its place first, [[SqlPlaces.id]]) and `grit.group_members`, each change made with one
  * `grit.visibility_changes` row, naming the turn a change was asked in when it was
  * ([[Administration.runFor]]). Settings carry no label, so it opens for
  * [[Subject.Public]]. A change's answer is read under the labels in force after it, read
  * again on the same transaction through `opener`.
  */
private[dbos] final class SqlAdministration(jot: Jot, opener: Opener) extends Administration {
  import SqlStatements.update

  def run(by: Account, room: Place, command: Command, at: Instant): Either[StoreError, Answer] =
    jot.write(Subject.Public)(within(by, room, command, at, None))

  def runFor(turn: TurnRef, command: Command, at: Instant): Either[StoreError, Answer] =
    jot.write(Subject.Public) { (tx: Tx^) ?=>
      asked(turn).flatMap {
        case Some((origin, first)) =>
          Administration.askedThrough(origin, first) match {
            case Some(by) => within(by, origin.room, command, at, Some(turn))
            case None => Right(Answer.Refused(Refusal.NotVouched))
          }
        case None => Right(Answer.Refused(Refusal.NotVouched))
      }
    }

  /** `turn`'s conversation's origin and the account that wrote its first entry, when that
    * entry is inbound (`grit.inbound`); `None` when the conversation is gone. Ids alone, no
    * entry's content, so the transaction's clearance bears on none of it.
    */
  private def asked(turn: TurnRef)(using
      tx: Tx^
  ): Either[StoreError, Option[(Origin, Option[Account])]] =
    // An id that is no UUID names no conversation: gone, rather than a failed cast.
    if (
      scala.util.Try(java.util.UUID.fromString(ConversationId.value(turn.conversationId))).isFailure
    )
      Right(None)
    else
      SqlStatements
        .one(
          """SELECT c.origin::text, i.account
            |  FROM grit.conversations c
            |  LEFT JOIN LATERAL (SELECT id FROM grit.entries
            |                      WHERE conversation_id = c.id AND turn_seq = ?
            |                      ORDER BY seq LIMIT 1) e ON true
            |  LEFT JOIN grit.inbound i ON i.entry_id = e.id
            | WHERE c.id = ?::uuid""".stripMargin
        ) { ps =>
          ps.setLong(1, TurnSeq.value(turn.turnSeq))
          ps.setString(2, ConversationId.value(turn.conversationId))
        }(rs => (rs.getString(1), Option(rs.getString(2))))
        .flatMap(_.fold(Right(None)) { (origin, account) =>
          for {
            o <- SqlConversationStore
              .readOrigin(ujson.read(origin))
              .left
              .map(why =>
                StoreError.Invalid(s"the origin of ${WorkflowId.value(turn.workflowId)}: $why")
              )
            a <- account.fold[Either[StoreError, Option[Account]]](Right(None))(
              SqlIdentities.read(_).map(Some(_))
            )
          } yield Some((o, a))
        })

  /** [[run]]'s body on `tx`, its audit row naming `turn`. */
  private def within(
      by: Account,
      room: Place,
      command: Command,
      at: Instant,
      turn: Option[TurnRef]
  )(using tx: Tx^): Either[StoreError, Answer] = {
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
              _ <- if (changed) audit(by, at, change, turn) else Right(())
              person <- change match {
                case Change.Clear(p, _) => SqlIdentities.person(p)
                case Change.Remove(p, _) => SqlIdentities.person(p)
                case Change.Relabel(_, _, _) | Change.Quiet(_, _) | Change.Baseline(_, _, _) =>
                  Right(None)
              }
              answer <- answered(allowed, person, room)
            } yield answer
        }
      }
    }
  }

  /** `kept`'s answer, made in `room`, under the labels in force read again on `tx`'s own
    * connection, which sees what this transaction has written.
    */
  private def answered(
      kept: grit.core.admin.Authority.Allowed,
      person: Option[Principal],
      room: Place
  )(using tx: Tx^): Either[StoreError, Answer] =
    opener.at(Tx.clearance(tx), Tx.connection(tx)).map { after =>
      Administration.answer(kept, person, room)(using after)
    }

  /** Keeps `change`; whether anything recorded changed (a repeat changes nothing). */
  private def keep(change: Change)(using tx: Tx^): Either[StoreError, Boolean] =
    change match {
      case Change.Relabel(room, _, Change.To.Set(label)) =>
        for {
          place <- SqlPlaces.id(room)
          id <- SqlLabels.intern(label)
          n <- update(
            """INSERT INTO grit.rooms (place_id, classification_id) VALUES (?::uuid, ?)
              |ON CONFLICT (place_id) DO UPDATE SET classification_id = EXCLUDED.classification_id
              | WHERE grit.rooms.classification_id IS DISTINCT FROM EXCLUDED.classification_id""".stripMargin
          ) { ps =>
            ps.setString(1, place)
            ps.setInt(2, id)
          }
        } yield n > 0
      case Change.Relabel(room, _, Change.To.Default(_)) =>
        atRoom(room, "classification_id = NULL", "r.classification_id IS NOT NULL")
      case Change.Quiet(room, true) =>
        SqlPlaces.id(room).flatMap { place =>
          update(
            """INSERT INTO grit.rooms (place_id, quiet) VALUES (?::uuid, true)
              |ON CONFLICT (place_id) DO UPDATE SET quiet = true WHERE NOT grit.rooms.quiet""".stripMargin
          )(_.setString(1, place)).map(_ > 0)
        }
      case Change.Quiet(room, false) => atRoom(room, "quiet = false", "r.quiet")
      case Change.Baseline(room, _, Some(to)) =>
        SqlPlaces.id(room).flatMap { place =>
          update(
            """INSERT INTO grit.rooms (place_id, baseline) VALUES (?::uuid, ?)
              |ON CONFLICT (place_id) DO UPDATE SET baseline = EXCLUDED.baseline
              | WHERE grit.rooms.baseline IS DISTINCT FROM EXCLUDED.baseline""".stripMargin
          ) { ps =>
            ps.setString(1, place)
            ps.setString(2, to.word)
          }.map(_ > 0)
        }
      case Change.Baseline(room, _, None) =>
        atRoom(room, "baseline = NULL", "r.baseline IS NOT NULL")
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

  /** One `grit.visibility_changes` row: `change`, made by `by` at `at`, asked in `turn`. */
  private def audit(by: Account, at: Instant, change: Change, turn: Option[TurnRef])(using
      Tx^
  ): Either[StoreError, Unit] =
    update(
      "INSERT INTO grit.visibility_changes (at, by, change, turn) VALUES (?, ?, ?::jsonb, ?)"
    ) { ps =>
      ps.setObject(1, at.atOffset(ZoneOffset.UTC))
      ps.setString(2, SqlIdentities.written(by))
      ps.setString(3, ChangeJson.write(change).render())
      ps.setString(4, turn.map(t => WorkflowId.value(t.workflowId)).orNull)
    }.map(_ => ())

  private def member(ps: PreparedStatement, group: GroupName, account: Account): Unit = {
    ps.setString(1, GroupName.value(group))
    ps.setString(2, SqlIdentities.written(account))
  }

  /** `sql` run with `bind`'s parameters; the rows it changed. */
}
