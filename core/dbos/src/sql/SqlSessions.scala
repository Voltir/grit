package grit.dbos.sql

import java.sql.ResultSet

import grit.core.id.{Harness, SessionId}
import grit.core.identity.Principal
import grit.core.session.{Event, Log, Session, Sessions}
import grit.core.store.{StoreError, Tx}

/** [[Sessions]] over `grit.sessions` and `grit.session_events`. */
private[dbos] final class SqlSessions extends Sessions {
  import SqlSessions.*

  def record(header: Session.Header, events: Vector[Event])(using
      tx: Tx^
  ): Either[StoreError, Sessions.Recorded] =
    events.minByOption(_.at) match {
      case None => Right(Sessions.Recorded(0, full = false))
      case Some(first) =>
        val ordered = events.sortBy(_.n).distinctBy(_.n)
        for {
          _ <- begin(header, first.at)
          row <- lock(header.id)
          known <- SqlStatements.many(
            """SELECT n FROM grit.session_events
              | WHERE session = ? AND n IN (SELECT jsonb_array_elements_text(?::jsonb)::bigint)""".stripMargin
          ) { ps =>
            ps.setLong(1, row.id)
            ps.setString(2, ujson.Arr.from(ordered.map(e => ujson.Str(e.n.toString))).render())
          }(_.getLong("n"))
          fresh = ordered.filterNot(e => known.contains(e.n))
          lines = Log.admit(row.next, fresh)
          logged = lines.collect { case Log.Line.Logged(_, e) => e }
          _ <- write(row.id, lines)
          _ <- update(row, fresh, logged, lines.size)
        } yield Sessions.Recorded(lines.size, row.next + lines.size > Log.HardCap)
    }

  def read(id: SessionId)(using Tx^): Either[StoreError, Option[Session]] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT s.*, ${SqlLabels.columns("l")}, p.kind,
           |       ${SqlIdentities.heldOf("s.principal_id")} AS held
           |  FROM grit.sessions s
           |  JOIN grit.classifications l ON l.id = s.classification_id
           |  JOIN grit.principals p ON p.id = s.principal_id
           | WHERE s.harness = ? AND s.local = ? AND ${SqlClearance.kept("s")}""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, Harness.value(id.harness))
        ps.setString(SqlClearance.Params + 2, id.local)
      }(sessionOf)
      .flatMap(found => SqlStatements.valid(found.map(_.left.map(_.toString))))
      .map(_.headOption)

  /** The row of `header`'s session inserted, started at `at`, unless it has one. */
  private def begin(header: Session.Header, at: java.time.Instant)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val clearance = Tx.clearance(tx)
    SqlLabels.intern(header.label.join(clearance.floor)).flatMap { label =>
      SqlStatements
        .update(
          s"""INSERT INTO grit.sessions
             |       (harness, local, parent_harness, parent_local, principal_id,
             |        classification_id, room_id, started_at, last_event_at)
             |VALUES (?, ?, ?, ?, ?, ?, ${SqlDocuments.RoomId}, ?, ?)
             |ON CONFLICT (harness, local) DO NOTHING""".stripMargin
        ) { ps =>
          ps.setString(1, Harness.value(header.id.harness))
          ps.setString(2, header.id.local)
          ps.setString(3, header.parent.map(p => Harness.value(p.harness)).orNull)
          ps.setString(4, header.parent.map(_.local).orNull)
          ps.setString(5, grit.core.id.PrincipalId.value(header.by.id))
          ps.setInt(6, label)
          ps.setString(7, SqlDocuments.room(clearance))
          ps.setObject(8, SqlDocuments.utc(at))
          ps.setObject(9, SqlDocuments.utc(at))
        }
        .map(_ => ())
    }
  }

  /** `id`'s row, locked until the transaction ends; `Invalid` when the transaction does not
    * read it.
    */
  private def lock(id: SessionId)(using Tx^): Either[StoreError, Row] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT s.id, s.next_seq, s.ended_at, ${SqlClearance.kept("s")} AS shown
           |  FROM grit.sessions s
           | WHERE s.harness = ? AND s.local = ?
           |   FOR UPDATE OF s""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, Harness.value(id.harness))
        ps.setString(SqlClearance.Params + 2, id.local)
      }(rs =>
        Option.when(rs.getBoolean("shown"))(
          Row(
            rs.getLong("id"),
            rs.getInt("next_seq"),
            Option(rs.getObject("ended_at", classOf[java.time.OffsetDateTime])).map(_.toInstant)
          )
        )
      )
      .flatMap(
        _.headOption.flatten.toRight(
          StoreError.Invalid(
            s"session ${Harness.value(id.harness)}:${id.local} is kept where this transaction does not read"
          )
        )
      )

  /** `lines` written to session `session`'s log. */
  private def write(session: Long, lines: Vector[Log.Line])(using Tx^): Either[StoreError, Unit] =
    if (lines.isEmpty) Right(())
    else
      SqlStatements
        .update(
          """INSERT INTO grit.session_events (session, seq, n, at, kind, shape)
            |SELECT ?, r.seq, r.n, r.at, r.kind, r.shape::json
            |  FROM jsonb_to_recordset(?::jsonb)
            |    AS r(seq integer, n bigint, at timestamptz, kind text, shape text)""".stripMargin
        ) { ps =>
          ps.setLong(1, session)
          ps.setString(2, ujson.Arr.from(lines.map(lineJson)).render())
        }
        .map(_ => ())

  /** `row` moved past `lines` new lines, `logged` the events among them and `fresh` every event
    * no line held before.
    */
  private def update(row: Row, fresh: Vector[Event], logged: Vector[Event], lines: Int)(using
      Tx^
  ): Either[StoreError, Unit] =
    fresh.maxByOption(_.at) match {
      case None => Right(())
      case Some(last) =>
        val counted = Session.Counters.of(logged)
        val summary = logged.reverseIterator.collectFirst {
          case Event(_, _, Event.Kind.Closing(s), _) =>
            Session.Summary.value(s)
        }
        SqlStatements
          .update(
            """UPDATE grit.sessions
              |   SET next_seq = ?, tool_calls = tool_calls + ?, edits = edits + ?,
              |       commits = commits + ?, bytes_read = bytes_read + ?,
              |       last_event_at = greatest(last_event_at, ?), ended_at = ?,
              |       summary = coalesce(?, summary)
              | WHERE id = ?""".stripMargin
          ) { ps =>
            ps.setInt(1, row.next + lines)
            ps.setLong(2, counted.toolCalls)
            ps.setLong(3, counted.edits)
            ps.setLong(4, counted.commits)
            ps.setLong(5, counted.bytesRead)
            ps.setObject(6, SqlDocuments.utc(last.at))
            ps.setObject(7, Session.ending(row.ended, logged).map(SqlDocuments.utc).orNull)
            ps.setString(8, summary.orNull)
            ps.setLong(9, row.id)
          }
          .map(_ => ())
    }
}

private[dbos] object SqlSessions {

  /** A session's row as a recording reads it under its lock. */
  private final case class Row(id: Long, next: Int, ended: Option[java.time.Instant])

  // As strings: a source's ordinal (a hook's nanoseconds) is past a JSON number's exact range.
  private def lineJson(line: Log.Line): ujson.Value = line match {
    case Log.Line.Logged(seq, e) =>
      ujson.Obj(
        "seq" -> seq,
        "n" -> ujson.Str(e.n.toString),
        "at" -> e.at.toString,
        "kind" -> line.kind,
        "shape" -> Event.Shape.value(e.shape)
      )
    case Log.Line.Overflow(seq, n, at) =>
      ujson.Obj(
        "seq" -> seq,
        "n" -> ujson.Str(n.toString),
        "at" -> at.toString,
        "kind" -> line.kind,
        "shape" -> Event.Shape.value(Event.Shape.Empty)
      )
  }

  private def sessionOf(rs: ResultSet): Either[String, Session] = {
    def id(h: String, l: String) = Harness.of(h).flatMap(SessionId.of(_, l))
    for {
      self <- id(rs.getString("harness"), rs.getString("local"))
      parent <- Option(rs.getString("parent_harness"))
        .zip(Option(rs.getString("parent_local")))
        .fold[Either[String, Option[SessionId]]](Right(None))((h, l) => id(h, l).map(Some(_)))
      by <- SqlIdentities
        .principal(
          rs.getString("principal_id"),
          rs.getString("kind"),
          Option(rs.getString("held")).getOrElse("[]")
        )
        .left
        .map(_.toString)
        .flatMap {
          case p: Principal.Person => Right(p)
          case Principal.Grit => Left("a session is run by a person, not grit")
        }
      summary <- Option(rs.getString("summary"))
        .fold[Either[String, Option[Session.Summary]]](Right(None))(
          Session.Summary.of(_).map(Some(_))
        )
      counters <- Session.Counters.read(
        rs.getLong("tool_calls"),
        rs.getLong("edits"),
        rs.getLong("commits"),
        rs.getLong("bytes_read")
      )
    } yield Session(
      self,
      parent,
      by,
      SqlLabels.read(rs),
      SqlDocuments.instant(rs, "started_at"),
      Option(rs.getObject("ended_at", classOf[java.time.OffsetDateTime])).map(_.toInstant),
      SqlDocuments.instant(rs, "last_event_at"),
      summary,
      counters,
      rs.getInt("next_seq") > Log.HardCap
    )
  }
}
