package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}
import java.time.{Instant, ZoneOffset}

import scala.util.Using

import grit.core.id.{ConversationId, EntryId}
import grit.core.place.Place
import grit.core.stitch.{Link, Placed, Said, StitchJson, StitchStore}
import grit.core.store.{StoreError, Tx}

/** [[StitchStore]] over `grit.stitches`, whose rows cascade from `grit.entries`, and the
  * entries of each room's conversations.
  */
final class SqlStitchStore extends StitchStore {
  import SqlEntryStore.attempt
  import SqlStitchStore.*

  def record(root: EntryId, placed: Placed, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Boolean] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      val first = Using.resource(
        conn.prepareStatement(
          """SELECT e.id = (SELECT f.id FROM grit.entries f
            |                WHERE f.conversation_id = e.conversation_id
            |                ORDER BY f.seq LIMIT 1)
            |  FROM grit.entries e WHERE e.id = ?""".stripMargin
        )
      ) { ps =>
        ps.setString(1, EntryId.value(root))
        Using.resource(ps.executeQuery())(rs => if (rs.next()) Some(rs.getBoolean(1)) else None)
      }
      first match {
        case None => Right(false)
        case Some(false) =>
          Left(StoreError.Invalid(s"${EntryId.value(root)} is not its conversation's first entry"))
        case Some(true) =>
          // From the entry's own row, so a gone entry inserts nothing.
          Using.resource(
            conn.prepareStatement(
              """INSERT INTO grit.stitches (entry_id, conversation_id, kind, root, at, placed)
                |SELECT id, conversation_id, ?, ?::uuid, ?, ?::jsonb FROM grit.entries WHERE id = ?
                |ON CONFLICT (entry_id) DO NOTHING""".stripMargin
            )
          ) { ps =>
            ps.setString(1, StitchJson.kindOf(placed))
            placed match {
              case f: Placed.Follows => ps.setString(2, ConversationId.value(f.root))
              case _: Placed.Begins | _: Placed.Unread => ps.setNull(2, java.sql.Types.VARCHAR)
            }
            ps.setObject(3, at.atOffset(ZoneOffset.UTC))
            ps.setString(4, StitchJson.write(placed).render())
            ps.setString(5, EntryId.value(root))
            Right(ps.executeUpdate() == 1)
          }
      }
    }.flatten
  }

  def placed(root: EntryId)(using tx: Tx^): Either[StoreError, Option[Placed]] =
    many("SELECT placed FROM grit.stitches WHERE entry_id = ?")(
      _.setString(1, EntryId.value(root))
    )(rs => readPlaced(rs.getString(1))).map(_.headOption)

  def spokenIn(room: Place, from: Instant, until: Instant)(using
      tx: Tx^
  ): Either[StoreError, Vector[Said]] =
    cleared(
      s"""WITH ${SqlClearance.With}
         |SELECT $SaidColumns
         |  FROM grit.entries e
         |  JOIN grit.conversations c ON c.id = e.conversation_id
         |  JOIN grit.places p ON p.id = c.place_id
         | WHERE p.path[1:cardinality(ARRAY(SELECT jsonb_array_elements_text(?::jsonb)))]
         |       = ARRAY(SELECT jsonb_array_elements_text(?::jsonb))
         |   AND e.created_at >= ? AND e.created_at < ?
         |   AND ($Spoken OR e.payload ->> 'kind' = 'closed')
         |   AND ${SqlClearance.entry("e")}
         | ORDER BY e.created_at, e.seq""".stripMargin
    ) { ps =>
      val path = ujson.Arr.from(room.segments.map(ujson.Str(_))).render()
      ps.setString(SqlClearance.Params + 1, path)
      ps.setString(SqlClearance.Params + 2, path)
      ps.setObject(SqlClearance.Params + 3, from.atOffset(ZoneOffset.UTC))
      ps.setObject(SqlClearance.Params + 4, until.atOffset(ZoneOffset.UTC))
    }(readSaid)

  def said(conversations: Vector[ConversationId], from: Instant, until: Instant)(using
      tx: Tx^
  ): Either[StoreError, Vector[Said]] =
    if (conversations.isEmpty) Right(Vector.empty)
    else
      cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT $SaidColumns
           |  FROM grit.entries e
           |  JOIN grit.conversations c ON c.id = e.conversation_id
           | WHERE e.conversation_id IN (SELECT jsonb_array_elements_text(?::jsonb)::uuid)
           |   AND e.created_at >= ? AND e.created_at < ?
           |   AND $Spoken AND ${SqlClearance.entry("e")}
           | ORDER BY e.created_at, e.seq""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, ids(conversations))
        ps.setObject(SqlClearance.Params + 2, from.atOffset(ZoneOffset.UTC))
        ps.setObject(SqlClearance.Params + 3, until.atOffset(ZoneOffset.UTC))
      }(readSaid)

  def openings(conversations: Vector[ConversationId])(using
      tx: Tx^
  ): Either[StoreError, Vector[Said]] =
    if (conversations.isEmpty) Right(Vector.empty)
    else
      cleared(
        s"""WITH ${SqlClearance.With}
           |SELECT $SaidColumns FROM (
           |  SELECT DISTINCT ON (f.conversation_id) f.*
           |    FROM grit.entries f
           |   WHERE f.conversation_id IN (SELECT jsonb_array_elements_text(?::jsonb)::uuid)
           |   ORDER BY f.conversation_id, f.seq
           |) e
           |  JOIN grit.conversations c ON c.id = e.conversation_id
           | WHERE $Spoken AND ${SqlClearance.entry("e")}""".stripMargin
      ) { ps =>
        ps.setString(SqlClearance.Params + 1, ids(conversations))
      }(readSaid).map { found =>
        conversations.flatMap(c => found.find(_.conversation == c))
      }

  def links(
      conversations: Vector[ConversationId]
  )(using tx: Tx^): Either[StoreError, Vector[Link]] =
    if (conversations.isEmpty) Right(Vector.empty)
    else
      many(
        """SELECT conversation_id, root FROM grit.stitches
          | WHERE kind = 'follows'
          |   AND (conversation_id IN (SELECT jsonb_array_elements_text(?::jsonb)::uuid)
          |        OR root IN (SELECT jsonb_array_elements_text(?::jsonb)::uuid))
          | ORDER BY at, entry_id""".stripMargin
      ) { ps =>
        ps.setString(1, ids(conversations))
        ps.setString(2, ids(conversations))
      }(rs => Link(ConversationId(rs.getString(1)), ConversationId(rs.getString(2))))

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

private object SqlStitchStore {

  /** An entry's columns, as [[SqlEntryStore]] reads them, and its conversation's origin. */
  val SaidColumns: String =
    "e.id, e.conversation_id, e.turn_seq, e.parent_id, e.seq, e.payload, e.created_at, c.origin"

  /** A person's message, to grit or heard, or grit's reply or post: what a strand is said in. */
  val Spoken: String =
    "(e.payload ->> 'kind' IN ('heard', 'posted') OR (e.payload ->> 'kind' = 'message' AND " +
      "e.payload -> 'message' ->> 'role' IN ('user', 'assistant')))"

  def ids(conversations: Vector[ConversationId]): String =
    ujson.Arr.from(conversations.map(c => ujson.Str(ConversationId.value(c)))).render()

  /** A row a store wrote that does not read is grit's own bug; `attempt` reports the throw. */
  def readPlaced(json: String): Placed =
    StitchJson.read(ujson.read(json)) match {
      case Right(p) => p
      case Left(why) => throw new IllegalStateException(s"unreadable placement: $why")
    }

  def readSaid(rs: ResultSet): Said = {
    val entry = SqlEntryStore.readEntry(rs)
    // The place is the origin's (Origin.place), as in SqlPeriodStore.openElsewhere.
    val place = SqlConversationStore.readOrigin(ujson.read(rs.getString("origin"))) match {
      case Right(o) => o.place
      case Left(why) => throw new IllegalStateException(s"unreadable origin: $why")
    }
    Said(entry.conversationId, place, entry)
  }

}
