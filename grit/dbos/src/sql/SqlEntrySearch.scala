package grit.dbos.sql

import scala.util.Using

import grit.core.id.{ConversationId, EntryId, TurnSeq}
import grit.core.store.{EntrySearch, StoreError, Tx}

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
                TurnSeq(rs.getLong("turn_seq")),
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
}
