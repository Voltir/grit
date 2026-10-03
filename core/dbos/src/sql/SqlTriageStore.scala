package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}
import java.time.{Instant, ZoneOffset}

import scala.util.Using

import grit.core.classify.AnswersJson
import grit.core.id.{ConversationId, EntryId, PeriodRef, PeriodSeq, TriageRef, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.store.{StoreError, Tx}
import grit.core.triage.{Tags, TriageStore}

/** [[TriageStore]] over `grit.triage`, whose rows cascade from `grit.entries`. */
final class SqlTriageStore extends TriageStore {
  import SqlEntryStore.attempt

  def record(
      entry: EntryId,
      tags: Tags,
      at: Instant
  )(using tx: Tx^): Either[StoreError, Boolean] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      // From the entry's own row, so a gone entry inserts nothing.
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.triage (entry_id, at, answers, model, input_tokens, output_tokens,
            |  cached_tokens, cost_usd, unanswered)
            |SELECT id, ?, ?::jsonb, ?, ?, ?, ?, ?, ? FROM grit.entries WHERE id = ?
            |ON CONFLICT (entry_id) DO NOTHING""".stripMargin
        )
      ) { ps =>
        ps.setObject(1, at.atOffset(ZoneOffset.UTC))
        bind(ps, tags)
        ps.setString(9, EntryId.value(entry))
        ps.executeUpdate() == 1
      }
    }
  }

  def of(entries: Vector[EntryId])(using tx: Tx^): Either[StoreError, Map[EntryId, Tags]] =
    if (entries.isEmpty) Right(Map.empty)
    else {
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      attempt {
        Using.resource(
          conn.prepareStatement(
            """SELECT entry_id, answers, model, input_tokens, output_tokens, cached_tokens,
              |       cost_usd, unanswered
              |  FROM grit.triage
              | WHERE entry_id IN (SELECT jsonb_array_elements_text(?::jsonb))""".stripMargin
          )
        ) { ps =>
          // The ids go as JSON, so no Java array crosses JDBC (separation checking).
          ps.setString(1, ujson.Arr.from(entries.map(e => ujson.Str(EntryId.value(e)))).render())
          Using.resource(ps.executeQuery()) { rs =>
            val rows = Map.newBuilder[EntryId, Tags]
            while (rs.next()) rows += EntryId(rs.getString("entry_id")) -> read(rs)
            rows.result()
          }
        }
      }
    }

  def tagged(from: Instant, until: Instant)(using
      tx: Tx^
  ): Either[StoreError, Vector[TriageStore.Tagged]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      // Each entry's period as SqlPeriodStore.of finds it; an entry in none (never one the
      // inbox heard) is left out, as the in-memory store leaves it.
      Using.resource(
        conn.prepareStatement(
          """SELECT t.entry_id, e.conversation_id, p.seq, e.turn_seq, t.at, t.answers, t.model,
            |       t.input_tokens, t.output_tokens, t.cached_tokens, t.cost_usd, t.unanswered
            |  FROM grit.triage t
            |  JOIN grit.entries e ON e.id = t.entry_id
            |  JOIN LATERAL (
            |       SELECT seq FROM grit.periods
            |        WHERE conversation_id = e.conversation_id AND first_turn <= e.turn_seq
            |          AND (last_turn IS NULL OR last_turn >= e.turn_seq)
            |        ORDER BY seq DESC LIMIT 1) p ON true
            | WHERE t.at >= ? AND t.at < ?
            | ORDER BY t.at, t.entry_id COLLATE "C"""".stripMargin
        )
      ) { ps =>
        ps.setObject(1, from.atOffset(ZoneOffset.UTC))
        ps.setObject(2, until.atOffset(ZoneOffset.UTC))
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[TriageStore.Tagged]
          while (rs.next()) {
            val period = PeriodSeq
              .of(rs.getLong("seq"))
              .getOrElse(throw new IllegalStateException(s"period ${rs.getLong("seq")}"))
            rows += TriageStore.Tagged(
              EntryId(rs.getString("entry_id")),
              TriageRef(
                PeriodRef(ConversationId(rs.getString("conversation_id")), period),
                TurnSeq(rs.getLong("turn_seq"))
              ),
              rs.getTimestamp("at").toInstant,
              read(rs)
            )
          }
          rows.result()
        }
      }
    }
  }

  /** Parameters 2 to 8 of `record`'s insert: `tags`' columns. */
  private def bind(ps: PreparedStatement, tags: Tags): Unit = tags match {
    case Tags.Weighed(answers, model, usage) =>
      ps.setString(2, AnswersJson.writeNamed(answers).render())
      ps.setString(3, model)
      ps.setLong(4, Tokens.value(usage.input))
      ps.setLong(5, Tokens.value(usage.output))
      ps.setLong(6, Tokens.value(usage.cachedInput))
      usage.costUsd match {
        case Some(c) => ps.setBigDecimal(7, c.bigDecimal)
        case None => ps.setNull(7, java.sql.Types.NUMERIC)
      }
      ps.setNull(8, java.sql.Types.VARCHAR)
    case Tags.Unanswered(why) =>
      ps.setNull(2, java.sql.Types.VARCHAR)
      ps.setNull(3, java.sql.Types.VARCHAR)
      (4 to 6).foreach(ps.setNull(_, java.sql.Types.BIGINT))
      ps.setNull(7, java.sql.Types.NUMERIC)
      ps.setString(8, why)
  }

  /** A row as its tags; one the schema's checks would refuse, or whose answers do not read,
    * is grit's own bug, which `attempt` reports.
    */
  private def read(rs: ResultSet): Tags =
    Option(rs.getString("unanswered")) match {
      case Some(why) => Tags.Unanswered(why)
      case None =>
        Tags.Weighed(
          AnswersJson
            .readNamed(ujson.read(rs.getString("answers")))
            .fold(why => throw new IllegalStateException(s"triage answers: $why"), identity),
          rs.getString("model"),
          Usage(
            Tokens(rs.getLong("input_tokens")),
            Tokens(rs.getLong("output_tokens")),
            Tokens(rs.getLong("cached_tokens")),
            Option(rs.getBigDecimal("cost_usd")).map(BigDecimal(_))
          )
        )
    }
}
