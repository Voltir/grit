package grit.dbos.sql

import java.sql.ResultSet
import java.time.{OffsetDateTime, ZoneOffset}

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.id.{ConversationId, EntryId, EntrySeq, TurnRef, TurnSeq}
import grit.core.store.{Entry, EntryStore, PayloadJson, StoreError, Tx}

import org.postgresql.util.PSQLException

/** [[EntryStore]] over the `grit.entries` table. */
final class SqlEntryStore extends EntryStore {
  import SqlEntryStore.*

  def insert(entry: Entry)(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val sql =
      s"""INSERT INTO grit.entries ($columns, label_id)
         |VALUES (?, ?::uuid, ?, ?, ?, ?::jsonb, ?,
         |        (SELECT label_id FROM grit.conversations WHERE id = ?::uuid))""".stripMargin
    // A failed statement aborts the whole transaction, and a `transact` step records its
    // output on the same connection afterwards. Rolling back to the savepoint keeps the
    // transaction usable, so `DuplicateId` can be recorded as the step's value.
    val savepoint = conn.setSavepoint()
    try {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        ps.setString(1, EntryId.value(entry.id))
        ps.setString(2, ConversationId.value(entry.conversationId))
        ps.setLong(3, TurnSeq.value(entry.turnSeq))
        ps.setString(4, entry.parentId.map(EntryId.value).orNull)
        ps.setLong(5, EntrySeq.value(entry.seq))
        ps.setString(6, PayloadJson.write(entry.payload).render())
        ps.setObject(7, entry.createdAt.atOffset(ZoneOffset.UTC))
        ps.setString(8, ConversationId.value(entry.conversationId))
        ps.executeUpdate()
      }
      // Past this entry, so its positions are not taken again once it is purged.
      Using.resource(
        conn.prepareStatement(
          """UPDATE grit.conversations
            |   SET next_turn = greatest(next_turn, ? + 1), next_seq = greatest(next_seq, ? + 1)
            | WHERE id = ?::uuid""".stripMargin
        )
      ) { ps =>
        ps.setLong(1, TurnSeq.value(entry.turnSeq))
        ps.setLong(2, EntrySeq.value(entry.seq))
        ps.setString(3, ConversationId.value(entry.conversationId))
        ps.executeUpdate()
      }
      conn.releaseSavepoint(savepoint)
      Right(())
    } catch {
      case e: PSQLException
          if e.getSQLState == UniqueViolation && violated(e).contains(PrimaryKey) =>
        conn.rollback(savepoint)
        Left(StoreError.DuplicateId(entry.id))
      case NonFatal(e) => Left(databaseError(e))
    }
  }

  def get(id: EntryId)(using tx: Tx^): Either[StoreError, Option[Entry]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val sql = s"""WITH ${SqlClearance.With}
                 |SELECT $columns FROM grit.entries e
                 | WHERE e.id = ? AND ${SqlClearance.entry("e")}""".stripMargin
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        SqlClearance.bind(ps, 1, Tx.clearance(tx))
        ps.setString(SqlClearance.Params + 1, EntryId.value(id))
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
      s"""WITH ${SqlClearance.With}
         |SELECT $columns FROM grit.entries e
         | WHERE e.conversation_id = ?::uuid AND ${SqlClearance.entry("e")}
         | ORDER BY e.seq""".stripMargin
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        SqlClearance.bind(ps, 1, Tx.clearance(tx))
        ps.setString(SqlClearance.Params + 1, ConversationId.value(conversation))
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

  def at(conversation: ConversationId, seqs: Vector[EntrySeq])(using
      tx: Tx^
  ): Either[StoreError, Vector[Entry]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val sql =
      s"""WITH ${SqlClearance.With}
         |SELECT $columns FROM grit.entries e
         | WHERE e.conversation_id = ?::uuid AND e.seq = ANY (?::bigint[])
         |   AND ${SqlClearance.entry("e")}
         | ORDER BY e.seq""".stripMargin
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        SqlClearance.bind(ps, 1, Tx.clearance(tx))
        ps.setString(SqlClearance.Params + 1, ConversationId.value(conversation))
        // An array literal of numbers, so no Java array is handed to the driver.
        ps.setString(SqlClearance.Params + 2, seqs.map(EntrySeq.value).mkString("{", ",", "}"))
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

  def ofTurn(turn: TurnRef)(using tx: Tx^): Either[StoreError, Vector[Entry]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    val sql =
      s"""WITH ${SqlClearance.With}
         |SELECT $columns FROM grit.entries e
         | WHERE e.conversation_id = ?::uuid AND e.turn_seq = ? AND ${SqlClearance.entry("e")}
         | ORDER BY e.seq""".stripMargin
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        SqlClearance.bind(ps, 1, Tx.clearance(tx))
        ps.setString(SqlClearance.Params + 1, ConversationId.value(turn.conversationId))
        ps.setLong(SqlClearance.Params + 2, TurnSeq.value(turn.turnSeq))
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

  def lockNext(
      conversation: ConversationId
  )(using tx: Tx^): Either[StoreError, EntryStore.Next] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          "SELECT next_turn, next_seq FROM grit.conversations WHERE id = ?::uuid FOR UPDATE"
        )
      ) { ps =>
        ps.setString(1, ConversationId.value(conversation))
        Using.resource(ps.executeQuery()) { rs =>
          // A conversation not (or no longer) recorded has nothing in it to come after.
          if (rs.next())
            EntryStore.Next(TurnSeq(rs.getLong("next_turn")), EntrySeq(rs.getLong("next_seq")))
          else EntryStore.Next(TurnSeq.First, EntrySeq.First)
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

  private[sql] def readEntry(rs: ResultSet): Entry =
    Entry(
      id = EntryId(rs.getString("id")),
      conversationId = ConversationId(rs.getString("conversation_id")),
      turnSeq = TurnSeq(rs.getLong("turn_seq")),
      parentId = Option(rs.getString("parent_id")).map(EntryId(_)),
      seq = EntrySeq(rs.getLong("seq")),
      // A payload that does not decode is grit's own bug, not a caller's
      // expected failure; `attempt` reports the throw as a DatabaseError.
      payload = PayloadJson.read(ujson.read(rs.getString("payload"))) match {
        case Right(p) => p
        case Left(why) => throw new IllegalStateException(s"undecodable payload: $why")
      },
      createdAt = rs.getObject("created_at", classOf[OffsetDateTime]).toInstant
    )
}
