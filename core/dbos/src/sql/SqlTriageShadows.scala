package grit.dbos.sql

import java.sql.ResultSet
import java.time.{Instant, ZoneOffset}

import scala.concurrent.duration.*
import scala.util.Using

import grit.core.id.{ConversationId, EntryId, PeriodRef, PeriodSeq, ShadowName, TriageRef, TurnSeq}
import grit.core.message.{Tokens, Usage}
import grit.core.store.{StoreError, Tx}
import grit.core.triage.{Shadowed, ShadowedJson, TriageShadows}

/** [[TriageShadows]] over `grit.triage_shadows`, whose rows cascade from `grit.entries`, and
  * `grit.triage`, whose tagged messages it offers.
  */
final class SqlTriageShadows extends TriageShadows {
  import SqlEntryStore.attempt

  def record(entry: EntryId, name: ShadowName, row: Shadowed, at: Instant)(using
      tx: Tx^
  ): Either[StoreError, Boolean] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      // From the entry's own row, so a gone entry inserts nothing.
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.triage_shadows (entry_id, name, at, request, latency_ms, failure,
            |  answers, requested, answered, input_tokens, output_tokens, cached_tokens, cost_usd)
            |SELECT id, ?, ?, ?, ?, ?, ?::jsonb, ?, ?, ?, ?, ?, ? FROM grit.entries WHERE id = ?
            |ON CONFLICT (entry_id, name) DO NOTHING""".stripMargin
        )
      ) { ps =>
        ps.setString(1, ShadowName.value(name))
        ps.setObject(2, at.atOffset(ZoneOffset.UTC))
        row match {
          case Shadowed.Answered(request, answers, usage, requested, answered, latency) =>
            ps.setString(3, request)
            ps.setLong(4, latency.toMillis)
            ps.setNull(5, java.sql.Types.VARCHAR)
            ps.setString(6, ShadowedJson.writeAnswers(answers).render())
            ps.setString(7, requested)
            ps.setString(8, answered)
            ps.setLong(9, Tokens.value(usage.input))
            ps.setLong(10, Tokens.value(usage.output))
            ps.setLong(11, Tokens.value(usage.cachedInput))
            usage.costUsd match {
              case Some(c) => ps.setBigDecimal(12, c.bigDecimal)
              case None => ps.setNull(12, java.sql.Types.NUMERIC)
            }
          case Shadowed.Failed(request, failure, latency) =>
            ps.setString(3, request)
            ps.setLong(4, latency.toMillis)
            ps.setString(5, ShadowedJson.kindWritten(failure))
            ps.setNull(6, java.sql.Types.VARCHAR)
            ps.setNull(7, java.sql.Types.VARCHAR)
            ps.setNull(8, java.sql.Types.VARCHAR)
            (9 to 11).foreach(ps.setNull(_, java.sql.Types.BIGINT))
            ps.setNull(12, java.sql.Types.NUMERIC)
        }
        ps.setString(13, EntryId.value(entry))
        ps.executeUpdate() == 1
      }
    }
  }

  def unshadowed(name: ShadowName, since: Instant, limit: Int)(using
      tx: Tx^
  ): Either[StoreError, Vector[TriageRef]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      // Each entry's period as SqlTriageStore.tagged finds it.
      Using.resource(
        conn.prepareStatement(
          """SELECT e.conversation_id, p.seq, e.turn_seq
            |  FROM grit.triage t
            |  JOIN grit.entries e ON e.id = t.entry_id
            |  JOIN LATERAL (
            |       SELECT seq FROM grit.periods
            |        WHERE conversation_id = e.conversation_id AND first_turn <= e.turn_seq
            |          AND (last_turn IS NULL OR last_turn >= e.turn_seq)
            |        ORDER BY seq DESC LIMIT 1) p ON true
            | WHERE t.at >= ?
            |   AND NOT EXISTS (SELECT 1 FROM grit.triage_shadows s
            |                    WHERE s.entry_id = t.entry_id AND s.name = ?)
            | ORDER BY t.at, t.entry_id COLLATE "C"
            | LIMIT ?""".stripMargin
        )
      ) { ps =>
        ps.setObject(1, since.atOffset(ZoneOffset.UTC))
        ps.setString(2, ShadowName.value(name))
        ps.setInt(3, limit.max(0))
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[TriageRef]
          while (rs.next()) {
            val period = PeriodSeq
              .of(rs.getLong("seq"))
              .getOrElse(throw new IllegalStateException(s"period ${rs.getLong("seq")}"))
            rows += TriageRef(
              PeriodRef(ConversationId(rs.getString("conversation_id")), period),
              TurnSeq(rs.getLong("turn_seq"))
            )
          }
          rows.result()
        }
      }
    }
  }

  def spent(name: ShadowName, from: Instant)(using tx: Tx^): Either[StoreError, BigDecimal] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          "SELECT COALESCE(SUM(cost_usd), 0) AS spent FROM grit.triage_shadows WHERE name = ? AND at >= ?"
        )
      ) { ps =>
        ps.setString(1, ShadowName.value(name))
        ps.setObject(2, from.atOffset(ZoneOffset.UTC))
        Using.resource(ps.executeQuery()) { rs =>
          rs.next()
          BigDecimal(rs.getBigDecimal("spent"))
        }
      }
    }
  }

  def recent(name: ShadowName, n: Int)(using tx: Tx^): Either[StoreError, Vector[BigDecimal]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(
        conn.prepareStatement(
          """SELECT cost_usd FROM grit.triage_shadows
            | WHERE name = ? AND cost_usd IS NOT NULL
            | ORDER BY at DESC, entry_id COLLATE "C" DESC
            | LIMIT ?""".stripMargin
        )
      ) { ps =>
        ps.setString(1, ShadowName.value(name))
        ps.setInt(2, n.max(0))
        Using.resource(ps.executeQuery()) { rs =>
          val costs = Vector.newBuilder[BigDecimal]
          while (rs.next()) costs += BigDecimal(rs.getBigDecimal("cost_usd"))
          costs.result()
        }
      }
    }
  }

  def of(name: ShadowName, entries: Vector[EntryId])(using
      tx: Tx^
  ): Either[StoreError, Map[EntryId, Shadowed]] =
    if (entries.isEmpty) Right(Map.empty)
    else {
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      attempt {
        Using.resource(
          conn.prepareStatement(
            """SELECT entry_id, request, latency_ms, failure, answers::text AS answers, requested,
              |       answered, input_tokens, output_tokens, cached_tokens, cost_usd
              |  FROM grit.triage_shadows
              | WHERE name = ? AND entry_id IN (SELECT jsonb_array_elements_text(?::jsonb))""".stripMargin
          )
        ) { ps =>
          ps.setString(1, ShadowName.value(name))
          // The ids go as JSON, so no Java array crosses JDBC (separation checking).
          ps.setString(2, ujson.Arr.from(entries.map(e => ujson.Str(EntryId.value(e)))).render())
          Using.resource(ps.executeQuery()) { rs =>
            val rows = Map.newBuilder[EntryId, Shadowed]
            while (rs.next()) rows += EntryId(rs.getString("entry_id")) -> read(rs)
            rows.result()
          }
        }
      }
    }

  /** A row as what the variant made of its message; one the schema's checks would refuse, or
    * whose answers do not read, is grit's own bug, which `attempt` reports.
    */
  private def read(rs: ResultSet): Shadowed = {
    val request = rs.getString("request")
    val latency = rs.getLong("latency_ms").millis
    Option(rs.getString("failure")) match {
      case Some(kind) =>
        Shadowed.Failed(
          request,
          ShadowedJson.kindRead(kind).fold(why => throw new IllegalStateException(why), identity),
          latency
        )
      case None =>
        Shadowed.Answered(
          request,
          ShadowedJson
            .readAnswers(ujson.read(rs.getString("answers")))
            .fold(why => throw new IllegalStateException(why), identity),
          Usage(
            Tokens(rs.getLong("input_tokens")),
            Tokens(rs.getLong("output_tokens")),
            Tokens(rs.getLong("cached_tokens")),
            Option(rs.getBigDecimal("cost_usd")).map(BigDecimal(_))
          ),
          rs.getString("requested"),
          rs.getString("answered"),
          latency
        )
    }
  }
}
