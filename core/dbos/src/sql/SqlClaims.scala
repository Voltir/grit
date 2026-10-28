package grit.dbos.sql

import java.sql.ResultSet
import java.time.Instant

import grit.core.claim.{Claim, ClaimHolder, ClaimRole, Claimed, Claims}
import grit.core.id.{DocKey, PluginName}
import grit.core.identity.Principal
import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Label

/** [[Claims]] over `grit.claims`, `plugin`'s. Two claims racing for one subject or holder at a
  * label never fail: the second waits for the first's transaction, then is refused.
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
    SqlLabels.intern(kept).flatMap { id =>
      SqlStatements
        .many(
          s"""INSERT INTO grit.claims
             |       (plugin, subject, holder, role, principal_id, made_at, classification_id, room_id)
             |VALUES (?, ?, ?, ?, ?, ?, ?, ${SqlDocuments.RoomId})
             |ON CONFLICT DO NOTHING RETURNING 1""".stripMargin
        ) { ps =>
          ps.setString(1, PluginName.value(plugin))
          ps.setString(2, DocKey.value(subject))
          ps.setString(3, ClaimHolder.value(holder))
          ps.setString(4, ClaimRole.value(role))
          ps.setString(5, grit.core.id.PrincipalId.value(by.id))
          ps.setObject(6, SqlDocuments.utc(at))
          ps.setInt(7, id)
          ps.setString(8, SqlDocuments.room(clearance))
        }(_ => ())
        .flatMap { inserted =>
          if (inserted.nonEmpty) Right(Claimed.Made(Claim(subject, holder, role, by, at, kept)))
          else conflict(subject, holder, id)
        }
    }
  }

  /** What refused a claim of `subject` for `holder` at the label whose id is `label`: a claim on
    * the subject before one by the holder, each named only when the transaction reads it.
    */
  private def conflict(subject: DocKey, holder: ClaimHolder, label: Int)(using
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
      }(rs =>
        claimOf(rs).map { c =>
          val shown = Option.when(rs.getBoolean("shown"))(c)
          if (rs.getBoolean("taken")) Claimed.Taken(shown) else Claimed.Holding(shown)
        }
      )
      .flatMap(SqlStatements.valid)
      .flatMap(
        _.headOption.toRight(
          StoreError.Invalid(s"a claim on ${DocKey.value(subject)} was refused, naming none")
        )
      )

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
}

private[dbos] object SqlClaims {

  /** Every claim of `plugin` deleted, as a disabled plugin's are. */
  def remove(plugin: PluginName)(using Tx^): Either[StoreError, Unit] =
    SqlStatements
      .update("DELETE FROM grit.claims WHERE plugin = ?")(_.setString(1, PluginName.value(plugin)))
      .map(_ => ())

  private val LabelId: String = SqlDocuments.LabelId

  /** `grit.claims` with each row's label's columns, for a read to select from. */
  private val Labelled: String =
    s"(SELECT c.*, ${SqlLabels.columns("l")} FROM grit.claims c " +
      "JOIN grit.classifications l ON l.id = c.classification_id)"

  /** A claim's columns, as [[claimOf]] reads them, from a row of [[Labelled]]'s shape. */
  private val Columns: String =
    "c.subject, c.holder, c.role, c.made_at, c.label_level, c.label_compartments, " +
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
      SqlLabels.read(rs)
    )
}
