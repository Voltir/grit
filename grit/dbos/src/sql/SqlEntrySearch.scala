package grit.dbos.sql

import scala.util.Using

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq}
import grit.core.store.{EntrySearch, OpenPeriod, StoreError, Tx}

/** [[EntrySearch]] by BM25 over `grit.entries.search_text` (ADR 0005). */
final class SqlEntrySearch extends EntrySearch {

  def search(
      conversation: ConversationId,
      from: TurnSeq,
      before: TurnSeq,
      query: String,
      limit: Int
  )(using
      tx: Tx^
  ): Either[StoreError, Vector[EntrySearch.Hit]] =
    if (query.isBlank || limit <= 0) Right(Vector.empty)
    else {
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      SqlEntryStore.attempt {
        Using.resource(conn.prepareStatement(SqlEntrySearch.Ranked)) { ps =>
          ps.setString(1, query)
          ps.setString(2, ConversationId.value(conversation))
          ps.setLong(3, TurnSeq.value(from))
          ps.setLong(4, TurnSeq.value(before))
          ps.setInt(5, limit)
          Using.resource(ps.executeQuery()) { rs =>
            val hits = Vector.newBuilder[EntrySearch.Hit]
            while (rs.next()) {
              hits += EntrySearch.Hit(
                EntryId(rs.getString("id")),
                TurnRef(conversation, TurnSeq(rs.getLong("turn_seq"))),
                -rs.getDouble("s")
              )
            }
            hits.result()
          }
        }
      }
    }

  def nearby(open: Vector[OpenPeriod], query: String, limit: Int)(using
      tx: Tx^
  ): Either[StoreError, Vector[EntrySearch.Hit]] =
    if (query.isBlank || limit <= 0 || open.isEmpty) Right(Vector.empty)
    else {
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      // The periods go as JSON, so no Java array crosses JDBC.
      val periods = ujson.Arr.from(open.map { o =>
        ujson.Obj(
          "c" -> ConversationId.value(o.conversation),
          "f" -> ujson.Num(TurnSeq.value(o.first).toDouble)
        )
      })
      SqlEntryStore.attempt {
        Using.resource(conn.prepareStatement(SqlEntrySearch.Nearby)) { ps =>
          ps.setString(1, query)
          ps.setString(2, periods.render())
          ps.setInt(3, limit)
          Using.resource(ps.executeQuery()) { rs =>
            val hits = Vector.newBuilder[EntrySearch.Hit]
            while (rs.next()) {
              hits += EntrySearch.Hit(
                EntryId(rs.getString("id")),
                TurnRef(
                  ConversationId(rs.getString("conversation_id")),
                  TurnSeq(rs.getLong("turn_seq"))
                ),
                -rs.getDouble("s")
              )
            }
            hits.result()
          }
        }
      }
    }

  def closings(conversations: Vector[ConversationId], query: String, limit: Int)(using
      tx: Tx^
  ): Either[StoreError, Vector[EntrySearch.Hit]] =
    if (query.isBlank || limit <= 0 || conversations.isEmpty) Right(Vector.empty)
    else {
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      // The ids go as JSON, so no Java array crosses JDBC.
      val ids = ujson.Arr.from(conversations.map(c => ujson.Str(ConversationId.value(c))))
      SqlEntryStore.attempt {
        Using.resource(conn.prepareStatement(SqlEntrySearch.Closings)) { ps =>
          ps.setString(1, query)
          ps.setString(2, ids.render())
          ps.setInt(3, limit)
          Using.resource(ps.executeQuery()) { rs =>
            val hits = Vector.newBuilder[EntrySearch.Hit]
            while (rs.next()) {
              hits += EntrySearch.Hit(
                EntryId(rs.getString("id")),
                TurnRef(
                  ConversationId(rs.getString("conversation_id")),
                  TurnSeq(rs.getLong("turn_seq"))
                ),
                -rs.getDouble("s")
              )
            }
            hits.result()
          }
        }
      }
    }
}

private object SqlEntrySearch {

  // pg_textsearch scores are negative, more negative is better. When the planner filters
  // to the conversation first, rows that do not match come back scored 0; the guard
  // drops them outside the LIMIT, so they never take a match's place (the spike's form C).
  private val Ranked =
    """SELECT id, turn_seq, s FROM (
      |  SELECT id, turn_seq, seq,
      |         search_text <@> to_bm25query(?, 'grit.idx_entries_bm25') AS s
      |    FROM grit.entries
      |   WHERE conversation_id = ?::uuid AND turn_seq >= ? AND turn_seq < ?
      |   ORDER BY s, seq DESC
      |   LIMIT ?
      |) ranked
      |WHERE s < 0
      |ORDER BY s, seq DESC""".stripMargin

  // Form C again, over several conversations, each from its open period's first turn: a
  // closing sits at the turn before, so it is never reached. Ties go latest first by time,
  // since seq orders only within one conversation.
  private val Nearby =
    """SELECT id, conversation_id, turn_seq, s FROM (
      |  SELECT e.id, e.conversation_id, e.turn_seq, e.created_at,
      |         e.search_text <@> to_bm25query(?, 'grit.idx_entries_bm25') AS s
      |    FROM grit.entries e
      |    JOIN jsonb_to_recordset(?::jsonb) AS r(c uuid, f bigint)
      |      ON e.conversation_id = r.c AND e.turn_seq >= r.f
      |   ORDER BY s, e.created_at DESC, e.id DESC
      |   LIMIT ?
      |) ranked
      |WHERE s < 0
      |ORDER BY s, created_at DESC, id DESC""".stripMargin

  // Form C over the closing entries of the conversations given, whatever their turn.
  private val Closings =
    """SELECT id, conversation_id, turn_seq, s FROM (
      |  SELECT e.id, e.conversation_id, e.turn_seq, e.created_at,
      |         e.search_text <@> to_bm25query(?, 'grit.idx_entries_bm25') AS s
      |    FROM grit.entries e
      |    JOIN jsonb_array_elements_text(?::jsonb) AS r(c) ON e.conversation_id = r.c::uuid
      |   WHERE e.payload ->> 'kind' = 'closed'
      |   ORDER BY s, e.created_at DESC, e.id DESC
      |   LIMIT ?
      |) ranked
      |WHERE s < 0
      |ORDER BY s, created_at DESC, id DESC""".stripMargin
}
