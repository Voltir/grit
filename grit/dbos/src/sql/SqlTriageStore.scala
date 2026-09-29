package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet, Types}
import java.time.{Instant, ZoneOffset}

import scala.util.Using

import grit.core.id.EntryId
import grit.core.message.{Tokens, Usage}
import grit.core.period.Probability
import grit.core.store.{StoreError, Tx}
import grit.core.triage.{Kind, Tags, TriageStore}

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
          """INSERT INTO grit.triage (entry_id, at, kind, kind_p, waiting, durable, helps, model,
            |  input_tokens, output_tokens, cached_tokens, cost_usd, unanswered)
            |SELECT id, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? FROM grit.entries WHERE id = ?
            |ON CONFLICT (entry_id) DO NOTHING""".stripMargin
        )
      ) { ps =>
        ps.setObject(1, at.atOffset(ZoneOffset.UTC))
        bind(ps, tags)
        ps.setString(13, EntryId.value(entry))
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
            """SELECT entry_id, kind, kind_p, waiting, durable, helps, model, input_tokens,
              |       output_tokens, cached_tokens, cost_usd, unanswered
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

  /** Parameters 2 to 12 of `record`'s insert: `tags`' columns. */
  private def bind(ps: PreparedStatement, tags: Tags): Unit = tags match {
    case Tags.Weighed(kind, kindP, waiting, durable, helps, model, usage) =>
      ps.setString(2, Kind.written(kind))
      Vector(kindP, waiting, durable, helps).zipWithIndex.foreach { (p: Probability, i: Int) =>
        ps.setDouble(3 + i, Probability.value(p))
      }
      ps.setString(7, model)
      ps.setLong(8, Tokens.value(usage.input))
      ps.setLong(9, Tokens.value(usage.output))
      ps.setLong(10, Tokens.value(usage.cachedInput))
      usage.costUsd match {
        case Some(c) => ps.setBigDecimal(11, c.bigDecimal)
        case None => ps.setNull(11, Types.NUMERIC)
      }
      ps.setNull(12, Types.VARCHAR)
    case Tags.Unanswered(why) =>
      ps.setNull(2, Types.VARCHAR)
      (3 to 6).foreach(ps.setNull(_, Types.DOUBLE))
      ps.setNull(7, Types.VARCHAR)
      (8 to 10).foreach(ps.setNull(_, Types.BIGINT))
      ps.setNull(11, Types.NUMERIC)
      ps.setString(12, why)
  }

  /** A row as its tags; one the schema's checks would refuse is grit's own bug, which
    * `attempt` reports.
    */
  private def read(rs: ResultSet): Tags =
    Option(rs.getString("unanswered")) match {
      case Some(why) => Tags.Unanswered(why)
      case None =>
        def p(column: String): Probability =
          Probability
            .of(rs.getDouble(column))
            .getOrElse(throw new IllegalStateException(s"triage $column is not a probability"))
        Tags.Weighed(
          Kind
            .read(rs.getString("kind"))
            .getOrElse(throw new IllegalStateException(s"unknown kind ${rs.getString("kind")}")),
          p("kind_p"),
          p("waiting"),
          p("durable"),
          p("helps"),
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
