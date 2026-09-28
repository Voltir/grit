package grit.dbos.sql

import java.time.{OffsetDateTime, ZoneOffset}

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.model.{CatalogJson, Profile}
import grit.core.store.{ModelSettingStore, StoreError, Tx}

/** [[ModelSettingStore]] over `grit.model_settings`. */
final class SqlModelSettingStore extends ModelSettingStore {

  def keep(settings: Profile, approvedBy: String, at: java.time.Instant)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    try {
      Using.resource(
        conn.prepareStatement(
          "INSERT INTO grit.model_settings (settings, approved_by, approved_at) VALUES (?::jsonb, ?, ?)"
        )
      ) { ps =>
        ps.setString(1, ujson.write(CatalogJson.writeProfile(settings)))
        ps.setString(2, approvedBy)
        ps.setObject(3, at.atOffset(ZoneOffset.UTC))
        ps.executeUpdate()
      }
      Right(())
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
  }

  def all()(using tx: Tx^): Either[StoreError, Vector[ModelSettingStore.Kept]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    try {
      val rows = Using.resource(
        conn.prepareStatement(
          "SELECT settings::text, approved_by, approved_at FROM grit.model_settings ORDER BY ordinal"
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val b = Vector.newBuilder[(String, String, java.time.Instant)]
          while (rs.next())
            b += ((
              rs.getString(1),
              rs.getString(2),
              rs.getObject(3, classOf[OffsetDateTime]).toInstant
            ))
          b.result()
        }
      }
      rows.foldLeft[Either[StoreError, Vector[ModelSettingStore.Kept]]](Right(Vector.empty)) {
        case (acc, (json, by, at)) =>
          acc.flatMap(kept =>
            CatalogJson
              .readProfile(ujson.read(json))
              .map(p => kept :+ ModelSettingStore.Kept(p, by, at))
              .left
              .map(why => StoreError.DatabaseError(s"a kept model fact is unreadable: $why"))
          )
      }
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
  }
}
