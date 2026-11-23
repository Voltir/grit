package grit.dbos.sql

import java.sql.ResultSet
import java.time.Instant

import grit.core.claim.{Claim, ClaimMoved, ClaimRole, Claimed, Claims}
import grit.core.id.{ClaimHolder, DocKey, Harness, PluginName, SessionId}
import grit.core.identity.Principal
import grit.core.session.Session
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label

/** [[Claims]] over `grit.claims`, `plugin`'s, leased on `grit.sessions` as the transaction reads
  * them. Two claims, or a claim and a move, racing for one subject or holder at a label never
  * fail: the second waits for the first's transaction, then is refused.
  */
private[dbos] final class SqlClaims(plugin: PluginName) extends Claims {
  import SqlClaims.*

  def claim(
      subject: DocKey,
      holder: ClaimHolder,
      role: ClaimRole,
      session: Option[SessionId],
      by: Principal.Person,
      label: Label,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Claimed] =
    SqlClaims.elsewhere(session, holder).flatMap {
      case Some((s, other)) => Right(Claimed.Elsewhere(s, other))
      case None => claiming(subject, holder, role, session, by, label, at, again = true)
    }

  /** [[claim]]; when the holder's own claim it found is released before its renewal, it claims
    * once more when `again`, else fails.
    */
  private def claiming(
      subject: DocKey,
      holder: ClaimHolder,
      role: ClaimRole,
      session: Option[SessionId],
      by: Principal.Person,
      label: Label,
      at: Instant,
      again: Boolean
  )(using tx: Tx^): Either[StoreError, Claimed] = {
    val clearance = Tx.clearance(tx)
    val kept = label.join(clearance.floor)
    val P = SqlClearance.Params
    for {
      id <- SqlLabels.intern(kept)
      _ <- lapse(subject, holder, id, at, onSubject = true)
      inserted <- SqlStatements
        .cleared(
          s"""WITH ${SqlClearance.With},
             |made AS (
             |  INSERT INTO grit.claims
             |         (plugin, subject, holder, role, principal_id, made_at, classification_id, room_id,
             |          session_harness, session_local)
             |  VALUES (?, ?, ?, ?, ?, ?, ?, ${SqlDocuments.RoomId}, ?, ?)
             |  ON CONFLICT DO NOTHING RETURNING *)
             |SELECT $Active AS active FROM made c""".stripMargin
        ) { ps =>
          ps.setString(P + 1, PluginName.value(plugin))
          ps.setString(P + 2, DocKey.value(subject))
          ps.setString(P + 3, ClaimHolder.value(holder))
          ps.setString(P + 4, ClaimRole.value(role))
          ps.setString(P + 5, grit.core.id.PrincipalId.value(by.id))
          ps.setObject(P + 6, SqlDocuments.utc(at))
          ps.setInt(P + 7, id)
          ps.setString(P + 8, SqlDocuments.room(clearance))
          SqlClaims.bindSession(ps, P + 9, session)
        }(SqlDocuments.instant(_, "active"))
      claimed <- inserted.headOption match {
        case Some(active) =>
          Right(Claimed.Made(Claim(subject, holder, role, by, at, kept, session, active)))
        case None =>
          conflict(subject, holder, role, session, id).flatMap {
            case Some(met) => Right(met)
            case None if again =>
              claiming(subject, holder, role, session, by, label, at, again = false)
            case None =>
              Left(
                StoreError.Invalid(
                  s"the claim on ${DocKey.value(subject)} was released twice while renewed"
                )
              )
          }
      }
    } yield claimed
  }

  /** The claims at the label whose id is `label` by `holder`, and on `subject` when `onSubject`
    * (else on any other subject), that have lapsed at `at`, as this transaction reads sessions,
    * deleted.
    */
  private def lapse(
      subject: DocKey,
      holder: ClaimHolder,
      label: Int,
      at: Instant,
      onSubject: Boolean
  )(using
      Tx^
  ): Either[StoreError, Unit] = {
    val whose =
      if (onSubject) "(c.subject = ? OR c.holder = ?)" else "c.subject <> ? AND c.holder = ?"
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With},
           |gone AS (
           |  DELETE FROM grit.claims c
           |   WHERE c.plugin = ? AND c.classification_id = ? AND $whose
           |     AND $Active <= ?
           |  RETURNING 1)
           |SELECT count(*) AS n FROM gone""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, PluginName.value(plugin))
        ps.setInt(SqlClearance.Params + 2, label)
        ps.setString(SqlClearance.Params + 3, DocKey.value(subject))
        ps.setString(SqlClearance.Params + 4, ClaimHolder.value(holder))
        ps.setObject(SqlClearance.Params + 5, SqlDocuments.utc(leased(at)))
      }(_ => ())
      .map(_ => ())
  }

  /** What a claim of `subject` for `holder` as `role`, worked by `session`, at the label whose id
    * is `label` met: the holder's own claim on the subject renewed when the transaction reads it
    * (or [[Claimed.HeldBy]] when another session than `session` works it), else a claim on the
    * subject before one by the holder, each named only when the transaction reads it; `None` when
    * the holder's own claim it read was released, or taken by a session, before it was renewed.
    */
  private def conflict(
      subject: DocKey,
      holder: ClaimHolder,
      role: ClaimRole,
      session: Option[SessionId],
      label: Int
  )(using Tx^): Either[StoreError, Option[Claimed]] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT c.subject = ? AS taken, ${SqlClearance.kept("c")} AS shown, $Columns
           |  FROM $Labelled c JOIN grit.principals p ON p.id = c.principal_id
           | WHERE c.plugin = ? AND c.classification_id = ? AND (c.subject = ? OR c.holder = ?)
           | ORDER BY taken DESC LIMIT 1""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, DocKey.value(subject))
        ps.setString(SqlClearance.Params + 2, PluginName.value(plugin))
        ps.setInt(SqlClearance.Params + 3, label)
        ps.setString(SqlClearance.Params + 4, DocKey.value(subject))
        ps.setString(SqlClearance.Params + 5, ClaimHolder.value(holder))
      }(rs => claimOf(rs).map(c => (c, rs.getBoolean("taken"), rs.getBoolean("shown"))))
      .flatMap(SqlStatements.valid)
      .flatMap(
        _.headOption.toRight(
          StoreError.Invalid(s"a claim on ${DocKey.value(subject)} was refused, naming none")
        )
      )
      .flatMap {
        case (c, true, true)
            if c.holder == holder && session.exists(s => c.session.exists(_ != s)) =>
          Right(Some(Claimed.HeldBy(c)))
        case (c, true, true) if c.holder == holder =>
          renew(c, role, c.session.orElse(session), label).map(_.map(Claimed.Made(_)))
        case (c, taken, shown) =>
          val named = Option.when(shown)(c)
          Right(Some(if (taken) Claimed.Taken(named) else Claimed.Holding(named)))
      }

  /** `claim`, kept at the label whose id is `label`, made `role` and worked by `session`: `None`
    * when it is gone, or another session works it than the one it was read with.
    */
  private def renew(claim: Claim, role: ClaimRole, session: Option[SessionId], label: Int)(using
      Tx^
  ): Either[StoreError, Option[Claim]] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With},
           |renewed AS (
           |  UPDATE grit.claims SET role = ?, session_harness = ?, session_local = ?
           |   WHERE plugin = ? AND subject = ? AND holder = ? AND made_at = ? AND classification_id = ?
           |     AND session_harness IS NOT DISTINCT FROM ? AND session_local IS NOT DISTINCT FROM ?
           |  RETURNING *)
           |SELECT $Active AS active FROM renewed c""".stripMargin
      ) { ps =>
        val P = SqlClearance.Params
        ps.setString(P + 1, ClaimRole.value(role))
        SqlClaims.bindSession(ps, P + 2, session)
        ps.setString(P + 4, PluginName.value(plugin))
        ps.setString(P + 5, DocKey.value(claim.subject))
        ps.setString(P + 6, ClaimHolder.value(claim.holder))
        ps.setObject(P + 7, SqlDocuments.utc(claim.made))
        ps.setInt(P + 8, label)
        SqlClaims.bindSession(ps, P + 9, claim.session)
      }(SqlDocuments.instant(_, "active"))
      .map(_.headOption.map(active => claim.copy(role = role, session = session, active = active)))

  def release(subject: DocKey, label: Label)(using tx: Tx^): Either[StoreError, Option[Claim]] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With},
           |gone AS (
           |  DELETE FROM grit.claims c
           |   WHERE c.plugin = ? AND c.subject = ? AND c.classification_id = $LabelId
           |     AND ${SqlClearance.kept("c")}
           |  RETURNING c.*)
           |SELECT $Columns
           |  FROM (SELECT g.*, ${SqlLabels.columns("l")}
           |          FROM gone g JOIN grit.classifications l ON l.id = g.classification_id) c
           |  JOIN grit.principals p ON p.id = c.principal_id""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, PluginName.value(plugin))
        ps.setString(SqlClearance.Params + 2, DocKey.value(subject))
        SqlLabels.bind(ps, SqlClearance.Params + 3, label)
      }(claimOf)
      .flatMap(SqlStatements.valid)
      .map(_.headOption)

  def moved(claim: Claim, holder: ClaimHolder, session: SessionId, role: ClaimRole, at: Instant)(
      using Tx^
  ): Either[StoreError, ClaimMoved] =
    SqlClaims.elsewhere(Some(session), holder).flatMap {
      case Some((s, away)) => Right(Claimed.Elsewhere(s, away))
      case None =>
        for {
          id <- SqlLabels.intern(claim.label)
          _ <- lapse(claim.subject, holder, id, at, onSubject = false)
          // Under a savepoint: a move breaking claims_holder, as `holder`'s claim on another
          // subject does (committed before it, or while it waited), fails and is undone, and the
          // claim it met is read.
          answer <- SqlSavepoints.atomic(move(claim, holder, session, role, at, id)) match {
            case Right(moved) => Right(moved)
            case Left(failed) =>
              holding(claim.subject, holder, id)
                .flatMap(
                  _.fold[Either[StoreError, ClaimMoved]](Left(failed))(h =>
                    Right(Claimed.Holding(h))
                  )
                )
          }
        } yield answer
    }

  /** `claim` moved as [[moved]] moves it, at the label whose id is `label`: [[ClaimMoved.Gone]] when no
    * row this transaction reads is it. Fails as the database does when the move breaks
    * `claims_holder`.
    */
  private def move(
      claim: Claim,
      holder: ClaimHolder,
      session: SessionId,
      role: ClaimRole,
      at: Instant,
      label: Int
  )(using Tx^): Either[StoreError, ClaimMoved] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With},
           |moved AS (
           |  UPDATE grit.claims c
           |     SET holder = ?, role = ?, session_harness = ?, session_local = ?, made_at = ?
           |   WHERE c.plugin = ? AND c.subject = ? AND c.holder = ? AND c.made_at = ?
           |     AND c.classification_id = ? AND ${SqlClearance.kept("c")}
           |  RETURNING c.*)
           |SELECT $Active AS active FROM moved c""".stripMargin
      ) { ps =>
        val P = SqlClearance.Params
        ps.setString(P + 1, ClaimHolder.value(holder))
        ps.setString(P + 2, ClaimRole.value(role))
        SqlClaims.bindSession(ps, P + 3, Some(session))
        ps.setObject(P + 5, SqlDocuments.utc(at))
        ps.setString(P + 6, PluginName.value(plugin))
        ps.setString(P + 7, DocKey.value(claim.subject))
        ps.setString(P + 8, ClaimHolder.value(claim.holder))
        ps.setObject(P + 9, SqlDocuments.utc(claim.made))
        ps.setInt(P + 10, label)
      }(SqlDocuments.instant(_, "active"))
      .map(
        _.headOption.fold(ClaimMoved.Gone)(active =>
          Claimed.Made(
            claim.copy(
              holder = holder,
              role = role,
              session = Some(session),
              made = at,
              active = active
            )
          )
        )
      )

  /** `holder`'s claim at the label whose id is `label` on a subject other than `subject`, named
    * only when this transaction reads it; `None` when it has none.
    */
  private def holding(subject: DocKey, holder: ClaimHolder, label: Int)(using
      Tx^
  ): Either[StoreError, Option[Option[Claim]]] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT ${SqlClearance.kept("c")} AS shown, $Columns
           |  FROM $Labelled c JOIN grit.principals p ON p.id = c.principal_id
           | WHERE c.plugin = ? AND c.classification_id = ? AND c.holder = ? AND c.subject <> ?""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, PluginName.value(plugin))
        ps.setInt(SqlClearance.Params + 2, label)
        ps.setString(SqlClearance.Params + 3, ClaimHolder.value(holder))
        ps.setString(SqlClearance.Params + 4, DocKey.value(subject))
      }(rs => claimOf(rs).map(c => Option.when(rs.getBoolean("shown"))(c)))
      .flatMap(SqlStatements.valid)
      .map(_.headOption)

  def live(at: Instant)(using Tx^): Either[StoreError, Vector[Claim]] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT $Columns
           |  FROM $Labelled c JOIN grit.principals p ON p.id = c.principal_id
           | WHERE c.plugin = ? AND ${SqlClearance.kept("c")} AND $Active > ?
           | ORDER BY c.subject, c.made_at""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, PluginName.value(plugin))
        ps.setObject(SqlClearance.Params + 2, SqlDocuments.utc(leased(at)))
      }(claimOf)
      .flatMap(SqlStatements.valid)

  def held(claim: Claim)(using Tx^): Either[StoreError, Option[Claim]] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With},
           |locked AS (
           |  SELECT c.* FROM grit.claims c
           |   WHERE c.plugin = ? AND c.subject = ? AND c.holder = ? AND c.made_at = ?
           |     AND c.classification_id = $LabelId AND ${SqlClearance.kept("c")}
           |     FOR UPDATE OF c)
           |SELECT $Columns
           |  FROM (SELECT k.*, ${SqlLabels.columns("l")}
           |          FROM locked k JOIN grit.classifications l ON l.id = k.classification_id) c
           |  JOIN grit.principals p ON p.id = c.principal_id""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, PluginName.value(plugin))
        ps.setString(SqlClearance.Params + 2, DocKey.value(claim.subject))
        ps.setString(SqlClearance.Params + 3, ClaimHolder.value(claim.holder))
        ps.setObject(SqlClearance.Params + 4, SqlDocuments.utc(claim.made))
        SqlLabels.bind(ps, SqlClearance.Params + 5, claim.label)
      }(claimOf)
      .flatMap(SqlStatements.valid)
      .map(_.headOption)

  def on(subject: DocKey, at: Instant)(using Tx^): Either[StoreError, Vector[Claim]] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With},
           |locked AS (
           |  SELECT c.* FROM grit.claims c
           |   WHERE c.plugin = ? AND c.subject = ? AND ${SqlClearance.kept("c")}
           |     FOR UPDATE OF c)
           |SELECT $Columns
           |  FROM (SELECT k.*, ${SqlLabels.columns("l")}
           |          FROM locked k JOIN grit.classifications l ON l.id = k.classification_id) c
           |  JOIN grit.principals p ON p.id = c.principal_id
           | WHERE $Active > ?
           | ORDER BY c.made_at, c.holder""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, PluginName.value(plugin))
        ps.setString(SqlClearance.Params + 2, DocKey.value(subject))
        ps.setObject(SqlClearance.Params + 3, SqlDocuments.utc(leased(at)))
      }(claimOf)
      .flatMap(SqlStatements.valid)

  def lock(subject: DocKey)(using Tx^): Either[StoreError, Unit] =
    SqlStatements
      .one("SELECT pg_advisory_xact_lock(?, hashtext(? || ' ' || ?))") { ps =>
        ps.setInt(1, SqlClaims.LockClass)
        ps.setString(2, PluginName.value(plugin))
        ps.setString(3, DocKey.value(subject))
      }(_ => ())
      .map(_ => ())
}

private[dbos] object SqlClaims {

  /** Every claim of `plugin` deleted, as a disabled plugin's are. */
  def remove(plugin: PluginName)(using Tx^): Either[StoreError, Unit] =
    SqlStatements
      .update("DELETE FROM grit.claims WHERE plugin = ?")(_.setString(1, PluginName.value(plugin)))
      .map(_ => ())

  /** Every claim lapsed longer than [[Session.Unended]] at `now` deleted, as the transaction
    * reads sessions: the sweep's, at maintenance.
    */
  def forget(now: Instant)(using Tx^): Either[StoreError, Unit] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With},
           |gone AS (DELETE FROM grit.claims c WHERE $Active <= ? RETURNING 1)
           |SELECT count(*) AS n FROM gone""".stripMargin
      )(ps =>
        ps.setObject(
          SqlClearance.Params + 1,
          SqlDocuments.utc(leased(now.minusMillis(Session.Unended.toMillis)))
        )
      )(_ => ())
      .map(_ => ())

  /** The advisory-lock class a plugin's subject is locked under ([[Claims.lock]]), with the hash
    * of the plugin and the subject: the bytes of `gclm`. A hash two pairs share only makes one
    * wait for the other.
    */
  val LockClass: Int = 0x67636c6d

  /** The latest a claim's [[Claim.active]] may be and have lapsed at `at`. */
  private def leased(at: Instant): Instant = at.minusMillis(Claims.Lease.toMillis)

  /** The `active` of the claim row `c`: its session's work, and that session's agents'
    * ([[Claim.active]]), as [[SqlClearance.With]]'s clearance reads them. GREATEST skips a NULL,
    * so a session with no agents, or ended, is its own last event.
    */
  private val Active: String =
    s"""GREATEST(c.made_at, CASE WHEN c.session_local IS NULL
       |  THEN (SELECT max(s.last_event_at) FROM grit.sessions s
       |         WHERE s.holder = c.holder AND ${SqlClearance.kept("s")})
       |  ELSE (SELECT GREATEST(s.last_event_at, CASE WHEN s.ended_at IS NULL THEN
       |                 (SELECT max(k.last_event_at) FROM grit.sessions k
       |                   WHERE k.parent_harness = s.harness AND k.parent_local = s.local
       |                     AND k.principal_id = s.principal_id
       |                     AND ${SqlClearance.kept("k")}) END)
       |          FROM grit.sessions s
       |         WHERE s.harness = c.session_harness AND s.local = c.session_local
       |           AND ${SqlClearance.kept("s")}) END)""".stripMargin

  /** What the recording of the session whose row id is `session`, once its row holds the events,
    * does to claims: once `ended`, every claim it works is worked by none; otherwise, when
    * `starting`, every claim of its holder that no session works is worked by it, lapsed or not.
    * Either way only claims kept at the session's own label and in its own room.
    */
  def recorded(session: Long, ended: Boolean, starting: Boolean)(using
      Tx^
  ): Either[StoreError, Unit] = {
    val same =
      "c.classification_id = s.classification_id AND c.room_id IS NOT DISTINCT FROM s.room_id"
    if (ended)
      SqlStatements
        .update(
          s"""UPDATE grit.claims c SET session_harness = NULL, session_local = NULL
             |  FROM grit.sessions s
             | WHERE s.id = ? AND c.session_harness = s.harness AND c.session_local = s.local
             |   AND $same""".stripMargin
        )(_.setLong(1, session))
        .map(_ => ())
    else if (starting)
      SqlStatements
        .cleared(
          s"""WITH ${SqlClearance.With},
             |taken AS (
             |  UPDATE grit.claims c SET session_harness = s.harness, session_local = s.local
             |    FROM grit.sessions s
             |   WHERE s.id = ? AND c.holder = s.holder AND c.session_local IS NULL AND $same
             |  RETURNING 1)
             |SELECT count(*) AS n FROM taken""".stripMargin
        )(_.setLong(SqlClearance.Params + 1, session))(_ => ())
        .map(_ => ())
    else Right(())
  }

  /** `session` bound to the two parameters from `i`, its harness then its local id. */
  private[sql] def bindSession(
      ps: java.sql.PreparedStatement,
      i: Int,
      session: Option[SessionId]
  ): Unit = {
    ps.setString(i, session.map(s => Harness.value(s.harness)).orNull)
    ps.setString(i + 1, session.map(_.local).orNull)
  }

  /** The session given and the holder it works as, when this transaction reads it working as
    * another than `holder`; `None` when it works as `holder`, as none, or is not read.
    */
  private def elsewhere(session: Option[SessionId], holder: ClaimHolder)(using
      Tx^
  ): Either[StoreError, Option[(SessionId, ClaimHolder)]] =
    session match {
      case None => Right(None)
      case Some(s) =>
        SqlStatements
          .cleared(
            s"""WITH ${SqlClearance.With}
               |SELECT s.holder FROM grit.sessions s
               | WHERE s.harness = ? AND s.local = ? AND s.holder IS NOT NULL
               |   AND ${SqlClearance.kept("s")}""".stripMargin
          )(ps => bindSession(ps, SqlClearance.Params + 1, Some(s)))(rs =>
            ClaimHolder.of(rs.getString("holder"))
          )
          .flatMap(SqlStatements.valid)
          .map(_.headOption.filter(_ != holder).map((s, _)))
    }

  private val LabelId: String = SqlDocuments.LabelId

  /** `grit.claims` with each row's label's columns, for a read to select from. */
  private val Labelled: String =
    s"(SELECT c.*, ${SqlLabels.columns("l")} FROM grit.claims c " +
      "JOIN grit.classifications l ON l.id = c.classification_id)"

  /** A claim's columns, as [[claimOf]] reads them, from a row of [[Labelled]]'s shape, in a
    * statement under [[SqlClearance.With]].
    */
  private val Columns: String =
    s"c.subject, c.holder, c.role, c.made_at, $Active AS active, c.label_level, c.label_compartments, " +
      s"c.principal_id, p.kind, ${SqlIdentities.heldOf("c.principal_id")} AS held, " +
      "c.session_harness, c.session_local"

  private def claimOf(rs: ResultSet): Either[String, Claim] =
    for {
      subject <- DocKey.of(rs.getString("subject"))
      holder <- ClaimHolder.of(rs.getString("holder"))
      role <- ClaimRole.of(rs.getString("role"))
      by <- SqlIdentities.person(rs, "a claim is made")
      session <- sessionOf(rs)
    } yield Claim(
      subject,
      holder,
      role,
      by,
      SqlDocuments.instant(rs, "made_at"),
      SqlLabels.read(rs),
      session,
      SqlDocuments.instant(rs, "active")
    )

  /** The session the row `rs` names in `session_harness` and `session_local`, none when NULL. */
  private def sessionOf(rs: ResultSet): Either[String, Option[SessionId]] =
    (Option(rs.getString("session_harness")), Option(rs.getString("session_local"))) match {
      case (Some(h), Some(l)) => Harness.of(h).flatMap(SessionId.of(_, l)).map(Some(_))
      case (None, None) => Right(None)
      case _ => Left("a claim names half a session")
    }
}
