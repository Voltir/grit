package grit.dbos.sql

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.id.{ConversationId, EntryId, TurnRef, TurnSeq, WorkflowId}
import grit.core.message.{Cost, Tokens, Usage}
import grit.core.spend.{Day, Spend, Spending}
import grit.core.store.{StoreError, Tx, UsageLedger}

import org.postgresql.util.PSQLException

/** [[UsageLedger]] over the `grit.usage_ledger` table, and the [[Spending]] read from it. */
final class SqlUsageLedger extends UsageLedger, Spending {

  def on(day: Day)(using tx: Tx^): Either[StoreError, Spend] =
    spent("created_at >= ? AND created_at < ?") { ps =>
      ps.setObject(1, java.time.OffsetDateTime.ofInstant(day.from, java.time.ZoneOffset.UTC))
      ps.setObject(2, java.time.OffsetDateTime.ofInstant(day.until, java.time.ZoneOffset.UTC))
    }

  def conversation(id: ConversationId)(using tx: Tx^): Either[StoreError, Spend] =
    spent("conversation_id = ?::uuid")(_.setString(1, ConversationId.value(id)))

  /** The rows `where` picks, its parameters set by `bind`, summed: an unpriced row makes the
    * cost a lower bound.
    */
  private def spent(where: String)(bind: java.sql.PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Spend] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    try {
      Using.resource(
        conn.prepareStatement(
          s"SELECT count(*), count(cost_usd), coalesce(sum(cost_usd), 0) FROM grit.usage_ledger WHERE $where"
        )
      ) { ps =>
        bind(ps)
        Using.resource(ps.executeQuery()) { rs =>
          val _ = rs.next()
          val (calls, priced, usd) = (rs.getInt(1), rs.getInt(2), BigDecimal(rs.getBigDecimal(3)))
          Right(Spend(calls, if (priced == calls) Cost.Exact(usd) else Cost.AtLeast(usd)))
        }
      }
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
  }

  def record(
      entry: EntryId,
      turn: TurnRef,
      workflow: WorkflowId,
      model: String,
      usage: Usage,
      estimatedInput: Tokens
  )(using tx: Tx^): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    // As in SqlEntryStore.insert: keeps the transaction usable after a duplicate.
    val savepoint = conn.setSavepoint()
    try {
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.usage_ledger
            |  (entry_id, workflow_id, model, input_tokens, output_tokens, cached_input_tokens, cost_usd,
            |   estimated_input_tokens, conversation_id, turn_seq)
            |VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?::uuid, ?)""".stripMargin
        )
      ) { ps =>
        ps.setString(1, EntryId.value(entry))
        ps.setString(2, WorkflowId.value(workflow))
        ps.setString(3, model)
        ps.setLong(4, Tokens.value(usage.input))
        ps.setLong(5, Tokens.value(usage.output))
        ps.setLong(6, Tokens.value(usage.cachedInput))
        ps.setBigDecimal(7, usage.costUsd.map(_.bigDecimal).orNull)
        ps.setLong(8, Tokens.value(estimatedInput))
        ps.setString(9, ConversationId.value(turn.conversationId))
        ps.setLong(10, TurnSeq.value(turn.turnSeq))
        ps.executeUpdate()
      }
      conn.releaseSavepoint(savepoint)
      Right(())
    } catch {
      case e: PSQLException if e.getSQLState == "23505" =>
        conn.rollback(savepoint)
        Left(StoreError.DuplicateId(entry))
      case NonFatal(e) => Left(SqlEntryStore.databaseError(e))
    }
  }

  def of(workflow: WorkflowId)(using tx: Tx^): Either[StoreError, Vector[UsageLedger.Row]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    try {
      Using.resource(
        conn.prepareStatement(
          """SELECT entry_id, model, input_tokens, output_tokens, cached_input_tokens, cost_usd,
            |       estimated_input_tokens
            |FROM grit.usage_ledger WHERE workflow_id = ? ORDER BY ordinal""".stripMargin
        )
      ) { ps =>
        ps.setString(1, WorkflowId.value(workflow))
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[UsageLedger.Row]
          while (rs.next()) {
            val usage = Usage(
              Tokens(rs.getLong(3)),
              Tokens(rs.getLong(4)),
              Tokens(rs.getLong(5)),
              Option(rs.getBigDecimal(6)).map(BigDecimal(_))
            )
            rows += UsageLedger.Row(
              EntryId(rs.getString(1)),
              rs.getString(2),
              usage,
              Tokens(rs.getLong(7))
            )
          }
          Right(rows.result())
        }
      }
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
  }

  def forget(conversation: ConversationId, from: TurnSeq, to: TurnSeq)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    try {
      Using.resource(
        conn.prepareStatement(
          // The speech decisions on those turns are ledger too (grit.speech).
          """WITH speech AS (
            |  DELETE FROM grit.speech
            |   WHERE conversation_id = ?::uuid AND turn_seq BETWEEN ? AND ?)
            |DELETE FROM grit.usage_ledger
            | WHERE conversation_id = ?::uuid AND turn_seq BETWEEN ? AND ?""".stripMargin
        )
      ) { ps =>
        Vector(0, 3).foreach { i =>
          ps.setString(i + 1, ConversationId.value(conversation))
          ps.setLong(i + 2, TurnSeq.value(from))
          ps.setLong(i + 3, TurnSeq.value(to))
        }
        ps.executeUpdate()
      }
      Right(())
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
  }
}
