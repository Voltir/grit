package grit.dbos

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.id.{EntryId, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.store.{StoreError, Tx, UsageLedger}

import org.postgresql.util.PSQLException

/** [[UsageLedger]] over the `grit.usage_ledger` table. */
final class SqlUsageLedger extends UsageLedger {

  def record(entry: EntryId, workflow: WorkflowId, model: String, usage: Usage)(using
      tx: Tx^
  ): Either[StoreError, Unit] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    // As in SqlEntryStore.insert: keeps the transaction usable after a duplicate.
    val savepoint = conn.setSavepoint()
    try {
      Using.resource(
        conn.prepareStatement(
          """INSERT INTO grit.usage_ledger
            |  (entry_id, workflow_id, model, input_tokens, output_tokens, cached_input_tokens, cost_usd)
            |VALUES (?, ?, ?, ?, ?, ?, ?)""".stripMargin
        )
      ) { ps =>
        ps.setString(1, EntryId.value(entry))
        ps.setString(2, WorkflowId.value(workflow))
        ps.setString(3, model)
        ps.setLong(4, Tokens.value(usage.input))
        ps.setLong(5, Tokens.value(usage.output))
        ps.setLong(6, Tokens.value(usage.cachedInput))
        ps.setBigDecimal(7, usage.costUsd.map(_.bigDecimal).orNull)
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
}
