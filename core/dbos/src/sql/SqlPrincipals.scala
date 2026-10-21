package grit.dbos.sql

import scala.util.Using

import grit.core.id.{EntryId, Href}
import grit.core.identity.Account
import grit.core.store.{Principals, Speakers, StoreError, Tx}

/** [[Principals]] over `grit.identities` (each account and the name it goes by),
  * `grit.authors` (who wrote each inbound entry) and `grit.inbound` (the link each was recorded
  * with).
  */
final class SqlPrincipals extends Principals {
  import SqlStatements.attempt

  def name(account: Account, name: String)(using tx: Tx^): Either[StoreError, Unit] =
    Principals.refusal(account, name) match {
      case Some(why) => Left(why)
      case None =>
        val conn: java.sql.Connection^{tx} = Tx.connection(tx)
        SqlIdentities.enroll(Set(account)).flatMap { _ =>
          attempt {
            Using.resource(
              conn.prepareStatement(
                """UPDATE grit.identities SET name = ?
                  | WHERE account = ? AND name IS DISTINCT FROM ?""".stripMargin
              )
            ) { ps =>
              ps.setString(1, name.trim)
              ps.setString(2, SqlIdentities.written(account))
              ps.setString(3, name.trim)
              ps.executeUpdate()
              ()
            }
          }
        }
    }

  def speakers(entries: Vector[EntryId])(using tx: Tx^): Either[StoreError, Speakers] =
    if (entries.isEmpty) Right(Speakers.none)
    else {
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      attempt {
        Using.resource(
          conn.prepareStatement(
            """SELECT a.entry_id, CASE WHEN a.kind = 'person' THEN n.name END, i.link
              |  FROM grit.authors a
              |  JOIN grit.inbound i ON i.entry_id = a.entry_id
              |  JOIN grit.identities n ON n.account = a.account
              | WHERE a.entry_id IN (SELECT jsonb_array_elements_text(?::jsonb))""".stripMargin
          )
        ) { ps =>
          // The ids go as JSON, so no Java array crosses JDBC (separation checking).
          ps.setString(1, ujson.Arr.from(entries.map(e => ujson.Str(EntryId.value(e)))).render())
          Using.resource(ps.executeQuery()) { rs =>
            val names = Map.newBuilder[EntryId, String]
            val links = Map.newBuilder[EntryId, Href]
            while (rs.next()) {
              val entry = EntryId(rs.getString(1))
              Option(rs.getString(2)).foreach(n => names += entry -> n)
              // Written from an Href (Recorder.authored), so it reads back as one.
              Option(rs.getString(3)).flatMap(Href.of).foreach(l => links += entry -> l)
            }
            Speakers(names.result(), links.result())
          }
        }
      }
    }
}
