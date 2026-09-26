package grit.dbos.sql

import java.time.{OffsetDateTime, ZoneOffset}

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.model.{CatalogJson, Profile}
import grit.core.store.{ModelFactStore, StoreError, Tx}

/** [[ModelFactStore]] over `grit.model_facts`. */
final class SqlModelFactStore extends ModelFactStore {

  def keep(facts: Profile, approvedBy: String, at: java.time.Instant)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    try {
      Using.resource(
        conn.prepareStatement(
          "INSERT INTO grit.model_facts (facts, approved_by, approved_at) VALUES (?::jsonb, ?, ?)"
        )
      ) { ps =>
        ps.setString(1, ujson.write(CatalogJson.writeProfile(facts)))
        ps.setString(2, approvedBy)
        ps.setObject(3, at.atOffset(ZoneOffset.UTC))
        ps.executeUpdate()
      }
      Right(())
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
  }

  def all()(using tx: Tx^): Either[StoreError, Vector[ModelFactStore.Kept]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    try {
      val rows = Using.resource(
        conn.prepareStatement(
          "SELECT facts::text, approved_by, approved_at FROM grit.model_facts ORDER BY ordinal"
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val b = Vector.newBuilder[(String, String, java.time.Instant)]
          while (rs.next()) b += ((rs.getString(1), rs.getString(2), rs.getObject(3, classOf[OffsetDateTime]).toInstant))
          b.result()
        }
      }
      rows.foldLeft[Either[StoreError, Vector[ModelFactStore.Kept]]](Right(Vector.empty)) {
        case (acc, (json, by, at)) =>
          acc.flatMap(kept =>
            CatalogJson
              .readProfile(ujson.read(json))
              .map(p => kept :+ ModelFactStore.Kept(p, by, at))
              .left
              .map(why => StoreError.DatabaseError(s"a kept model fact is unreadable: $why"))
          )
      }
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
  }
}
