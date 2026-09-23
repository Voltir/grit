package grit.core.interop

import grit.core.{Entry, EntryId, EntryStore, StoreError, Tx}
import java.sql.{ResultSet, SQLException}
import java.time.{OffsetDateTime, ZoneOffset}
import scala.util.Using
import scala.util.control.NonFatal

/** [[EntryStore]] over the `grit.entries` table. */
final class SqlEntryStore extends EntryStore {
  import SqlEntryStore.*

  def insert(entry: Entry)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val sql =
      s"""INSERT INTO grit.entries ($columns)
         |VALUES (?, ?, ?, ?::jsonb, ?)""".stripMargin
    try {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setString(1, EntryId.value(entry.id))
        ps.setString(2, entry.parentId.map(EntryId.value).orNull)
        ps.setLong(3, entry.seq)
        ps.setString(4, entry.payload.render())
        ps.setObject(5, entry.createdAt.atOffset(ZoneOffset.UTC))
        ps.executeUpdate()
      }
      Right(())
    } catch {
      case e: SQLException if e.getSQLState == UniqueViolation =>
        Left(StoreError.DuplicateId(entry.id))
      case NonFatal(e) => Left(databaseError(e))
    }
  }

  def get(id: EntryId)(using tx: Tx^): Either[StoreError, Option[Entry]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val sql = s"SELECT $columns FROM grit.entries WHERE id = ?"
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setString(1, EntryId.value(id))
        Using.resource(ps.executeQuery()) { rs =>
          if (rs.next()) Some(readEntry(rs)) else None
        }
      }
    }
  }

  def listAll()(using tx: Tx^): Either[StoreError, Vector[Entry]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val sql = s"SELECT $columns FROM grit.entries ORDER BY seq, id"
    attempt {
      Using.resource(conn.createStatement()) { stmt =>
        Using.resource(stmt.executeQuery(sql)) { rs =>
          val rows = Vector.newBuilder[Entry]
          while (rs.next()) {
            rows += readEntry(rs)
          }
          rows.result()
        }
      }
    }
  }
}

private object SqlEntryStore {

  /** Column order shared by every statement and by `readEntry`. */
  private val columns = "id, parent_id, seq, payload, created_at"

  /** Postgres SQLSTATE for a primary key or unique index violation. */
  private val UniqueViolation = "23505"

  private def attempt[A](body: => A): Either[StoreError, A] =
    try Right(body)
    catch { case NonFatal(e) => Left(databaseError(e)) }

  /** `getMessage` is `null` for some driver exceptions; rule 6 stops here. */
  private def databaseError(e: Throwable): StoreError.DatabaseError =
    StoreError.DatabaseError(Option(e.getMessage).getOrElse(e.toString))

  private def readEntry(rs: ResultSet): Entry =
    Entry(
      id = EntryId(rs.getString("id")),
      parentId = Option(rs.getString("parent_id")).map(EntryId(_)),
      seq = rs.getLong("seq"),
      payload = ujson.read(rs.getString("payload")),
      createdAt = rs.getObject("created_at", classOf[OffsetDateTime]).toInstant
    )
}
