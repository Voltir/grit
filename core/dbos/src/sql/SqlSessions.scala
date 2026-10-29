package grit.dbos.sql

import java.sql.ResultSet
import java.time.Instant

import grit.core.id.{ClaimHolder, Harness, SessionId}
import grit.core.identity.Principal
import grit.core.place.Place
import grit.core.retention.Target
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
          _ <- header.holder.fold[Either[StoreError, Unit]](Right(()))(holding(row, _))
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
          _ <-
            if (logged.exists(_.kind == Event.Kind.Commit)) SqlNotices.send(SqlNotices.Sessions, "")
            else Right(())
        } yield Sessions.Recorded(lines.size, row.next + lines.size > Log.HardCap)
    }

  def read(id: SessionId)(using Tx^): Either[StoreError, Option[Session]] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT s.*, array_to_json(s.place)::text AS place_json, ${SqlLabels.columns(
            "l"
          )}, p.kind,
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

  def lately(id: SessionId)(using Tx^): Either[StoreError, Option[Session.Lately]] =
    read(id).flatMap {
      case None => Right(None)
      case Some(session) =>
        for {
          commits <- SqlStatements.many(
            s"""SELECT e.at, e.shape->>'${Event.Shape.Sha}' AS sha,
               |       e.shape->>'${Event.Shape.Subject}' AS subject
               |  FROM grit.session_events e
               | WHERE e.session = ${SqlSessions.Keyed} AND e.kind = ?
               |   AND e.shape->>'${Event.Shape.Sha}' IS NOT NULL
               |   AND NOT ${SqlSessions.ByAgent}
               | ORDER BY e.seq DESC LIMIT ?""".stripMargin
          ) { ps =>
            ps.setString(1, Harness.value(id.harness))
            ps.setString(2, id.local)
            ps.setString(3, Event.Kind.Commit.name)
            ps.setInt(4, Session.Lately.Most)
          }(rs =>
            Session.Lately.Commit(
              SqlDocuments.instant(rs, "at"),
              rs.getString("sha"),
              Option(rs.getString("subject"))
            )
          )
          byAgents <- SqlStatements.one(
            s"""SELECT count(*) AS n FROM grit.session_events e
               | WHERE e.session = ${SqlSessions.Keyed} AND e.kind = ?
               |   AND e.shape->>'${Event.Shape.Sha}' IS NOT NULL
               |   AND ${SqlSessions.ByAgent}""".stripMargin
          ) { ps =>
            ps.setString(1, Harness.value(id.harness))
            ps.setString(2, id.local)
            ps.setString(3, Event.Kind.Commit.name)
          }(_.getInt("n"))
          edited <- named(id, Event.Kind.Edited)
          read <- named(id, Event.Kind.Read(0))
        } yield Some(Session.Lately(session, commits, byAgents.getOrElse(0), edited, read))
    }

  /** The last [[Session.Lately.Most]] distinct paths, else names, each with its worktree, among
    * `id`'s last [[Session.Lately.Scanned]] events of `kind`, newest first; a worktree that reads
    * as no place is left off.
    */
  private def named(id: SessionId, kind: Event.Kind)(using
      Tx^
  ): Either[StoreError, Vector[Session.Lately.Touched]] =
    SqlStatements.many(
      s"""SELECT w.p, w.wt FROM (
         |  SELECT coalesce(e.shape->>'${Event.Shape.Path}', e.shape->>'${Event.Shape.Named}') AS p,
         |         e.shape->>'${Event.Shape.Worktree}' AS wt, e.seq
         |    FROM grit.session_events e
         |   WHERE e.session = ${SqlSessions.Keyed} AND e.kind = ?
         |   ORDER BY e.seq DESC LIMIT ?
         |) w
         | WHERE w.p IS NOT NULL
         | GROUP BY w.p, w.wt ORDER BY max(w.seq) DESC LIMIT ?""".stripMargin
    ) { ps =>
      ps.setString(1, Harness.value(id.harness))
      ps.setString(2, id.local)
      ps.setString(3, kind.name)
      ps.setInt(4, Session.Lately.Scanned)
      ps.setInt(5, Session.Lately.Most)
    }(rs =>
      Session.Lately.Touched(
        rs.getString("p"),
        Option(rs.getString("wt")).flatMap(Place.read(_).toOption)
      )
    )

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
             |        classification_id, room_id, place, started_at, last_event_at, holder)
             |VALUES (?, ?, ?, ?, ?, ?, ${SqlDocuments.RoomId},
             |        ARRAY(SELECT jsonb_array_elements_text(?::jsonb)), ?, ?, ?)
             |ON CONFLICT (harness, local) DO NOTHING""".stripMargin
        ) { ps =>
          ps.setString(1, Harness.value(header.id.harness))
          ps.setString(2, header.id.local)
          ps.setString(3, header.parent.map(p => Harness.value(p.harness)).orNull)
          ps.setString(4, header.parent.map(_.local).orNull)
          ps.setString(5, grit.core.id.PrincipalId.value(header.by.id))
          ps.setInt(6, label)
          ps.setString(7, SqlDocuments.room(clearance))
          ps.setString(8, SqlDocuments.path(header.at).render())
          ps.setObject(9, SqlDocuments.utc(at))
          ps.setObject(10, SqlDocuments.utc(at))
          ps.setString(11, header.holder.map(ClaimHolder.value).orNull)
        }
        .map(_ => ())
    }
  }

  /** `row` working as `holder` from now on, when it works as none yet. */
  private def holding(row: Row, holder: ClaimHolder)(using Tx^): Either[StoreError, Unit] =
    SqlStatements
      .update("UPDATE grit.sessions SET holder = ? WHERE id = ? AND holder IS NULL") { ps =>
        ps.setString(1, ClaimHolder.value(holder))
        ps.setLong(2, row.id)
      }
      .map(_ => ())

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

  /** The row id of the session its two parameters name, its harness then its local id. */
  private val Keyed: String = "(SELECT id FROM grit.sessions WHERE harness = ? AND local = ?)"

  /** Whether the event `e`'s shape marks it made by an agent the session started. */
  private val ByAgent: String = s"coalesce(e.shape->>'${Event.Shape.Child}' = 'true', false)"

  /** When the session `s` counts as ended ([[Session.ends]]), its one parameter
    * [[Session.Unended]] in seconds.
    */
  private val Ends: String =
    "coalesce(s.ended_at, s.last_event_at + make_interval(secs => ?))"

  /** Up to `n` sessions whose log has been kept [[Session.LogKept]] since they ended at `now`,
    * oldest ended first, as [[Target.SessionEvents]]; then up to `n` whose row has been kept
    * [[Session.Kept]], as [[Target.Session]].
    */
  def due(now: Instant, n: Int)(using Tx^): Either[StoreError, Vector[Target]] =
    for {
      logs <- endedBefore(now.minusMillis(Session.LogKept.toMillis), n, logged = true)
      rows <- endedBefore(now.minusMillis(Session.Kept.toMillis), n, logged = false)
    } yield logs.map(Target.SessionEvents(_)) ++ rows.map(Target.Session(_))

  /** `id`'s log deleted, when it has been kept [[Session.LogKept]] since the session ended at
    * `now`, or the session is gone; `false`, deleting nothing, when it has not.
    */
  def forgetLog(id: SessionId, now: Instant)(using Tx^): Either[StoreError, Boolean] =
    whenEnded(id, now.minusMillis(Session.LogKept.toMillis))(
      "DELETE FROM grit.session_events WHERE session = ?"
    )

  /** `id`'s row and any log it still has deleted, when it has been kept [[Session.Kept]] since
    * the session ended at `now`, or the session is gone; `false`, deleting nothing, when not.
    */
  def forget(id: SessionId, now: Instant)(using Tx^): Either[StoreError, Boolean] =
    whenEnded(id, now.minusMillis(Session.Kept.toMillis))("DELETE FROM grit.sessions WHERE id = ?")

  /** `delete`, bound to `id`'s row's id, run when the session ended before `before`, its row
    * locked against a recording meanwhile; whether it ran, or the session is gone.
    */
  private def whenEnded(id: SessionId, before: Instant)(delete: String)(using
      Tx^
  ): Either[StoreError, Boolean] =
    SqlStatements
      .one(
        s"""SELECT s.id, $Ends <= ? AS ended FROM grit.sessions s
           | WHERE s.harness = ? AND s.local = ? FOR UPDATE""".stripMargin
      ) { ps =>
        ps.setLong(1, Session.Unended.toSeconds)
        ps.setObject(2, SqlDocuments.utc(before))
        ps.setString(3, Harness.value(id.harness))
        ps.setString(4, id.local)
      }(rs => (rs.getLong("id"), rs.getBoolean("ended")))
      .flatMap {
        case None => Right(true)
        case Some((_, false)) => Right(false)
        case Some((row, true)) =>
          SqlStatements.update(delete)(_.setLong(1, row)).map(_ => true)
      }

  /** Up to `n` sessions that ended before `before`, oldest ended first; with `logged`, only
    * those whose log holds a line.
    */
  private def endedBefore(before: Instant, n: Int, logged: Boolean)(using
      Tx^
  ): Either[StoreError, Vector[SessionId]] =
    SqlStatements
      .many(
        s"""SELECT s.harness, s.local FROM grit.sessions s
           | WHERE $Ends <= ?
           |   AND (NOT ? OR EXISTS (SELECT 1 FROM grit.session_events e WHERE e.session = s.id))
           | ORDER BY $Ends, s.id LIMIT ?""".stripMargin
      ) { ps =>
        ps.setLong(1, Session.Unended.toSeconds)
        ps.setObject(2, SqlDocuments.utc(before))
        ps.setBoolean(3, logged)
        ps.setLong(4, Session.Unended.toSeconds)
        ps.setInt(5, n max 0)
      }(rs => Harness.of(rs.getString("harness")).flatMap(SessionId.of(_, rs.getString("local"))))
      .flatMap(SqlStatements.valid)

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
      at <- SqlDocuments.placeOf(rs.getString("place_json"))
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
      holder <- Option(rs.getString("holder"))
        .fold[Either[String, Option[ClaimHolder]]](Right(None))(ClaimHolder.of(_).map(Some(_)))
    } yield Session(
      self,
      parent,
      by,
      SqlLabels.read(rs),
      at,
      SqlDocuments.instant(rs, "started_at"),
      Option(rs.getObject("ended_at", classOf[java.time.OffsetDateTime])).map(_.toInstant),
      SqlDocuments.instant(rs, "last_event_at"),
      summary,
      counters,
      rs.getInt("next_seq") > Log.HardCap,
      holder
    )
  }
}
