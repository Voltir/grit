package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}
import java.time.{Instant, ZoneOffset}

import scala.util.Using

import grit.core.id.{ConversationId, EntryId, PrincipalId, PrincipalIds}
import grit.core.place.Place
import grit.core.recipe.RoomReads
import grit.core.stitch.Said
import grit.core.store.{StoreError, Tx}

/** [[RoomReads]] over `grit.entries`, each conversation's place, and `grit.inbound`. */
final class SqlRoomReads extends RoomReads {
  import SqlEntryStore.attempt
  import SqlStitchStore.{SaidColumns, Spoken, ids, readSaid}

  def said(room: Place, from: Instant, until: Instant, outside: Set[ConversationId], most: Int)(
      using tx: Tx^
  ): Either[StoreError, Vector[Said]] =
    latest("", room, from, until, outside, most)(_ => ())

  def saidBy(
      room: Place,
      author: PrincipalId,
      from: Instant,
      until: Instant,
      outside: Set[ConversationId],
      most: Int
  )(using tx: Tx^): Either[StoreError, Vector[Said]] =
    latest(
      "JOIN grit.inbound i ON i.entry_id = e.id AND i.author = ?",
      room,
      from,
      until,
      outside,
      most
    )(_.setString(SqlClearance.Params + 1, PrincipalId.value(author)))

  def author(entry: EntryId)(using tx: Tx^): Either[StoreError, Option[PrincipalId]] =
    many("SELECT author FROM grit.inbound WHERE entry_id = ?")(
      _.setString(1, EntryId.value(entry))
    )(rs => PrincipalIds.stored(rs.getString(1))).map(_.headOption)

  /** The latest `most` messages in `room` its transaction reads, joined by `join`, whose one
    * parameter, if any, `bind` sets first after the clearance's.
    */
  private def latest(
      join: String,
      room: Place,
      from: Instant,
      until: Instant,
      outside: Set[ConversationId],
      most: Int
  )(bind: PreparedStatement => Unit)(using tx: Tx^): Either[StoreError, Vector[Said]] =
    if (most < 1) Right(Vector.empty)
    else {
      val first = SqlClearance.Params + (if (join.isEmpty) 1 else 2)
      cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT $SaidColumns
           |  FROM grit.entries e
           |  JOIN grit.conversations c ON c.id = e.conversation_id
           |  JOIN grit.places p ON p.id = c.place_id
           |  $join
           | WHERE p.path[1:cardinality(ARRAY(SELECT jsonb_array_elements_text(?::jsonb)))]
           |       = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))
           |   AND e.created_at >= ? AND e.created_at < ?
           |   AND e.conversation_id NOT IN (SELECT jsonb_array_elements_text(?::jsonb)::uuid)
           |   AND $Spoken AND ${SqlClearance.entry("e")}
           | ORDER BY e.created_at DESC, e.id COLLATE "C" DESC
           | LIMIT ?""".stripMargin
      ) { ps =>
        bind(ps)
        val path = ujson.Arr.from(room.segments.map(ujson.Str(_))).render()
        ps.setString(first, path)
        ps.setString(first + 1, path)
        ps.setObject(first + 2, from.atOffset(ZoneOffset.UTC))
        ps.setObject(first + 3, until.atOffset(ZoneOffset.UTC))
        ps.setString(first + 4, ids(outside.toVector))
        ps.setInt(first + 5, most)
      }(readSaid)
    }

  /** As [[many]], for a statement beginning `WITH ${SqlClearance.With}`: the clearance's
    * parameters are set first, to the transaction's, then `bind`'s.
    */
  private def cleared[A](sql: String)(bind: PreparedStatement => Unit)(read: ResultSet => A)(using
      tx: Tx^
  ): Either[StoreError, Vector[A]] = {
    val clearance = Tx.clearance(tx)
    many(sql) { ps =>
      SqlClearance.bind(ps, 1, clearance)
      bind(ps)
    }(read)
  }

  private def many[A](sql: String)(bind: PreparedStatement => Unit)(read: ResultSet => A)(using
      tx: Tx^
  ): Either[StoreError, Vector[A]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        bind(ps)
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[A]
          while (rs.next()) rows += read(rs)
          rows.result()
        }
      }
    }
  }
}
