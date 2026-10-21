package grit.dbos.sql

import java.sql.PreparedStatement
import java.time.{Instant, ZoneOffset}

import grit.core.id.{ConversationId, EntryId, PrincipalId, PrincipalIds}
import grit.core.place.Place
import grit.core.recipe.RoomReads
import grit.core.stitch.Said
import grit.core.store.{StoreError, Tx}

/** [[RoomReads]] over `grit.entries`, each conversation's place, and `grit.authors`. */
final class SqlRoomReads extends RoomReads {
  import SqlStatements.{cleared, many}
  import SqlStitchStore.{SaidColumns, Spoken, ids, readSaid}

  def said(room: Place, from: Instant, until: Instant, outside: Set[ConversationId], most: Int)(
      using tx: Tx^
  ): Either[StoreError, Vector[Said]] =
    ordered("", room, from, until, outside, most, latest = true)(_ => ())

  def saidBy(
      room: Place,
      author: PrincipalId,
      from: Instant,
      until: Instant,
      outside: Set[ConversationId],
      most: Int
  )(using tx: Tx^): Either[StoreError, Vector[Said]] =
    by(author, room, from, until, outside, most, latest = true)

  def firstBy(
      room: Place,
      author: PrincipalId,
      from: Instant,
      until: Instant,
      outside: Set[ConversationId],
      most: Int
  )(using tx: Tx^): Either[StoreError, Vector[Said]] =
    by(author, room, from, until, outside, most, latest = false)

  /** [[ordered]] over the messages written through any account linked to `author` now. */
  private def by(
      author: PrincipalId,
      room: Place,
      from: Instant,
      until: Instant,
      outside: Set[ConversationId],
      most: Int,
      latest: Boolean
  )(using tx: Tx^): Either[StoreError, Vector[Said]] =
    ordered(
      "JOIN grit.authors a ON a.entry_id = e.id AND a.principal_id = ?",
      room,
      from,
      until,
      outside,
      most,
      latest
    )(_.setString(SqlClearance.Params + 1, PrincipalId.value(author)))

  def author(entry: EntryId)(using tx: Tx^): Either[StoreError, Option[PrincipalId]] =
    many("SELECT principal_id FROM grit.authors WHERE entry_id = ?")(
      _.setString(1, EntryId.value(entry))
    )(rs => PrincipalIds.stored(rs.getString(1))).map(_.headOption)

  /** The `most` messages in `room` its transaction reads, joined by `join`, whose one
    * parameter, if any, `bind` sets first after the clearance's: the latest first when
    * `latest`, else the earliest first.
    */
  private def ordered(
      join: String,
      room: Place,
      from: Instant,
      until: Instant,
      outside: Set[ConversationId],
      most: Int,
      latest: Boolean
  )(bind: PreparedStatement => Unit)(using tx: Tx^): Either[StoreError, Vector[Said]] =
    if (most < 1) Right(Vector.empty)
    else {
      val first = SqlClearance.Params + (if (join.isEmpty) 1 else 2)
      val order = if (latest) "DESC" else "ASC"
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
           | ORDER BY e.created_at $order, e.id COLLATE "C" $order
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

}
