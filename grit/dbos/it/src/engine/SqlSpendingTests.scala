package grit.dbos.engine

import java.time.Instant

import scala.util.Using

import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.spend.SpendingContract
import grit.core.store.Tx
import grit.dbos.sql.{LiveDb, SqlUsageLedger, TestPostgres}

/** The spending contract, kept by the SQL ledger against a real Postgres. */
object SqlSpendingTests extends SpendingContract {

  // Opening an engine applies schema.sql; nothing here launches DBOS.
  private lazy val config = {
    val c = TestPostgres.freshDatabase("sql_spending")
    LiveEngine.open(c, "test").close()
    c
  }

  private val sql = new SqlUsageLedger()

  protected def ledger: SqlUsageLedger = sql

  protected def recordAt(
      entry: EntryId,
      turn: TurnRef,
      workflow: WorkflowId,
      usd: Option[String],
      at: Instant
  ): Unit =
    LiveDb.transaction(config) { (tx: Tx^) ?=>
      val usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, usd.map(BigDecimal(_)))
      sql
        .record(entry, turn, workflow, "m", usage, Tokens(1))
        .fold(e => sys.error(e.toString), identity)
      // A row is recorded at the transaction's time; the contract needs chosen ones.
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement("UPDATE grit.usage_ledger SET created_at = ? WHERE entry_id = ?")
      ) { ps =>
        ps.setObject(1, java.time.OffsetDateTime.ofInstant(at, java.time.ZoneOffset.UTC))
        ps.setString(2, EntryId.value(entry))
        val _ = ps.executeUpdate()
      }
    }

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)
}
