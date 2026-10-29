package grit.dbos.sql

import java.sql.ResultSet
import java.time.Instant

import grit.core.claim.{Claim, ClaimRole, Claimed, Claims}
import grit.core.id.{ClaimHolder, DocKey, PluginName}
import grit.core.identity.Principal
import grit.core.session.Session
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label

/** [[Claims]] over `grit.claims`, `plugin`'s, leased on `grit.sessions` as the transaction reads
  * them. Two claims racing for one subject or holder at a label never fail: the second waits for
  * the first's transaction, then is refused.
  */
private[dbos] final class SqlClaims(plugin: PluginName) extends Claims {
  import SqlClaims.*

  def claim(
      subject: DocKey,
      holder: ClaimHolder,
      role: ClaimRole,
      by: Principal.Person,
      label: Label,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Claimed] = {
    val clearance = Tx.clearance(tx)
    val kept = label.join(clearance.floor)
    val P = SqlClearance.Params
    for {
      id <- SqlLabels.intern(kept)
      _ <- lapse(subject, holder, id, at)
      inserted <- SqlStatements
        .cleared(
          s"""WITH ${SqlClearance.With},
             |made AS (
             |  INSERT INTO grit.claims
             |         (plugin, subject, holder, role, principal_id, made_at, classification_id, room_id)
             |  VALUES (?, ?, ?, ?, ?, ?, ?, ${SqlDocuments.RoomId})
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
        }(SqlDocuments.instant(_, "active"))
      claimed <- inserted.headOption match {
        case Some(active) => Right(Claimed.Made(Claim(subject, holder, role, by, at, kept, active)))
        case None => conflict(subject, holder, role, id)
      }
    } yield claimed
  }

  /** The claims at the label whose id is `label` on `subject` or by `holder` that have lapsed at
    * `at`, as this transaction reads sessions, deleted.
    */
  private def lapse(subject: DocKey, holder: ClaimHolder, label: Int, at: Instant)(using
      Tx^
  ): Either[StoreError, Unit] =
    SqlStatements
      .cleared(
        s"""WITH ${SqlClearance.With},
           |gone AS (
           |  DELETE FROM grit.claims c
           |   WHERE c.plugin = ? AND c.classification_id = ? AND (c.subject = ? OR c.holder = ?)
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

  /** What a claim of `subject` for `holder` as `role` at the label whose id is `label` met: the
    * holder's own claim on the subject renewed when the transaction reads it, else a claim on
    * the subject before one by the holder, each named only when the transaction reads it.
    */
  private def conflict(subject: DocKey, holder: ClaimHolder, role: ClaimRole, label: Int)(using
      Tx^
  ): Either[StoreError, Claimed] =
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
        case (c, true, true) if c.holder == holder => renew(c, role, label).map(Claimed.Made(_))
        case (c, taken, shown) =>
          val named = Option.when(shown)(c)
          Right(if (taken) Claimed.Taken(named) else Claimed.Holding(named))
      }

  /** `claim`, kept at the label whose id is `label`, made `role`. */
  private def renew(claim: Claim, role: ClaimRole, label: Int)(using
      Tx^
  ): Either[StoreError, Claim] =
    SqlStatements
      .update(
        "UPDATE grit.claims SET role = ? WHERE plugin = ? AND subject = ? AND classification_id = ?"
      ) { ps =>
        ps.setString(1, ClaimRole.value(role))
        ps.setString(2, PluginName.value(plugin))
        ps.setString(3, DocKey.value(claim.subject))
        ps.setInt(4, label)
      }
      .map(_ => claim.copy(role = role))

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

  /** The latest a claim's [[Claim.active]] may be and have lapsed at `at`. */
  private def leased(at: Instant): Instant = at.minusMillis(Claims.Lease.toMillis)

  /** [[Claim.active]] of the claim row `c`: the latest of its making and the last events of the
    * sessions working as its holder that [[SqlClearance.With]]'s clearance reads.
    */
  private val Active: String =
    s"""GREATEST(c.made_at, (SELECT max(s.last_event_at) FROM grit.sessions s
       |                    WHERE s.holder = c.holder AND ${SqlClearance.kept("s")}))""".stripMargin

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
      s"c.principal_id, p.kind, ${SqlIdentities.heldOf("c.principal_id")} AS held"

  private def claimOf(rs: ResultSet): Either[String, Claim] =
    for {
      subject <- DocKey.of(rs.getString("subject"))
      holder <- ClaimHolder.of(rs.getString("holder"))
      role <- ClaimRole.of(rs.getString("role"))
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
          case Principal.Grit => Left("a claim is made by a person, not grit")
        }
    } yield Claim(
      subject,
      holder,
      role,
      by,
      SqlDocuments.instant(rs, "made_at"),
      SqlLabels.read(rs),
      SqlDocuments.instant(rs, "active")
    )
}
