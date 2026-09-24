package grit.dbos.sql

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.id.{EntryId, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.store.{StoreError, Tx, UsageLedger}

import org.postgresql.util.PSQLException

/** [[UsageLedger]] over the `grit.usage_ledger` table. */
final class SqlUsageLedger extends UsageLedger {

  def record(
      entry: EntryId,
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
            |   estimated_input_tokens)
            |VALUES (?, ?, ?, ?, ?, ?, ?, ?)""".stripMargin
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
            |FROM grit.usage_ledger WHERE workflow_id = ? ORDER BY created_at, entry_id""".stripMargin
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
}
