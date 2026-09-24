package grit.dbos

import grit.core.{ConversationId, Entry, EntryId, EntryStore, PayloadJson, StoreError, Tx, TurnSeq}
import java.sql.ResultSet
import org.postgresql.util.PSQLException
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
         |VALUES (?, ?::uuid, ?, ?, ?, ?::jsonb, ?)""".stripMargin
    try {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setString(1, EntryId.value(entry.id))
        ps.setString(2, ConversationId.value(entry.conversationId))
        ps.setLong(3, TurnSeq.value(entry.turnSeq))
        ps.setString(4, entry.parentId.map(EntryId.value).orNull)
        ps.setLong(5, entry.seq)
        ps.setString(6, PayloadJson.write(entry.payload).render())
        ps.setObject(7, entry.createdAt.atOffset(ZoneOffset.UTC))
        ps.executeUpdate()
      }
      Right(())
    } catch {
      case e: PSQLException
          if e.getSQLState == UniqueViolation && violated(e).contains(PrimaryKey) =>
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

  def list(
      conversation: ConversationId
  )(using tx: Tx^): Either[StoreError, Vector[Entry]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val sql =
      s"SELECT $columns FROM grit.entries WHERE conversation_id = ?::uuid ORDER BY seq"
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setString(1, ConversationId.value(conversation))
        Using.resource(ps.executeQuery()) { rs =>
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

private[dbos] object SqlEntryStore {

  /** Column order shared by every statement and by `readEntry`. */
  private val columns =
    "id, conversation_id, turn_seq, parent_id, seq, payload, created_at"

  /** Postgres SQLSTATE for a primary key or unique index violation. */
  private val UniqueViolation = "23505"

  /** The primary key's name. Any other unique violation, such as a seq taken by a writer
    * that skipped the conversation lock, is a database error, not a duplicate id.
    */
  private val PrimaryKey = "entries_pkey"

  private def violated(e: PSQLException): Option[String] =
    Option(e.getServerErrorMessage).flatMap(m => Option(m.getConstraint))

  private[dbos] def attempt[A](body: => A): Either[StoreError, A] =
    try Right(body)
    catch { case NonFatal(e) => Left(databaseError(e)) }

  /** `getMessage` is `null` for some driver exceptions; rule 6 stops here. */
  private[dbos] def databaseError(e: Throwable): StoreError.DatabaseError =
    StoreError.DatabaseError(Option(e.getMessage).getOrElse(e.toString))

  private def readEntry(rs: ResultSet): Entry =
    Entry(
      id = EntryId(rs.getString("id")),
      conversationId = ConversationId(rs.getString("conversation_id")),
      turnSeq = TurnSeq(rs.getLong("turn_seq")),
      parentId = Option(rs.getString("parent_id")).map(EntryId(_)),
      seq = rs.getLong("seq"),
      // A payload that does not decode is grit's own bug, not a caller's
      // expected failure; `attempt` reports the throw as a DatabaseError.
      payload = PayloadJson.read(ujson.read(rs.getString("payload"))) match {
        case Right(p) => p
        case Left(why) => throw new IllegalStateException(s"undecodable payload: $why")
      },
      createdAt = rs.getObject("created_at", classOf[OffsetDateTime]).toInstant
    )
}
