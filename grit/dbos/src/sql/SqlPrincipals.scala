package grit.dbos.sql

import scala.util.Using

import grit.core.id.{EntryId, PrincipalId}
import grit.core.store.{Principals, Speakers, StoreError, Tx}

/** [[Principals]] over `grit.principals` (a person's name) and `grit.inbound` (who wrote each
  * inbound entry).
  */
final class SqlPrincipals extends Principals {
  import SqlEntryStore.attempt

  def enroll(id: PrincipalId, name: String)(using tx: Tx^): Either[StoreError, Unit] =
    Principals.refusal(id, name) match {
      case Some(why) => Left(why)
      case None =>
        val conn: java.sql.Connection^{tx} = Tx.connection(tx)
        attempt {
          Using.resource(
            conn.prepareStatement(
              """INSERT INTO grit.principals (id, kind, name) VALUES (?, 'person', ?)
                |ON CONFLICT (id) DO UPDATE SET name = EXCLUDED.name""".stripMargin
            )
          ) { ps =>
            ps.setString(1, PrincipalId.value(id))
            ps.setString(2, name.trim)
            ps.executeUpdate()
            ()
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
            """SELECT i.entry_id, p.name FROM grit.inbound i
              |JOIN grit.principals p ON p.id = i.author
              |WHERE i.entry_id IN (SELECT jsonb_array_elements_text(?::jsonb))
              |  AND p.name IS NOT NULL""".stripMargin
          )
        ) { ps =>
          // The ids go as JSON, so no Java array crosses JDBC (separation checking).
          ps.setString(1, ujson.Arr.from(entries.map(e => ujson.Str(EntryId.value(e)))).render())
          Using.resource(ps.executeQuery()) { rs =>
            val names = Map.newBuilder[EntryId, String]
            while (rs.next()) names += EntryId(rs.getString(1)) -> rs.getString(2)
            Speakers(names.result())
          }
        }
      }
    }
}
