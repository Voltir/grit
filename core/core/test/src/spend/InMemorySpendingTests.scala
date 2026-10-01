package grit.core.spend

import java.time.Instant

import grit.core.id.{EntryId, TurnRef, WorkflowId}
import grit.core.message.{Tokens, Usage}
import grit.core.store.{InMemoryUsageLedger, Tx}
import grit.dbos.sql.TestTx

/** The spending contract, kept by the in-memory ledger. */
object InMemorySpendingTests extends SpendingContract {

  private val memory = new InMemoryUsageLedger

  protected def ledger: InMemoryUsageLedger = memory

  protected def recordAt(
      entry: EntryId,
      turn: TurnRef,
      workflow: WorkflowId,
      usd: Option[String],
      at: Instant
  ): Unit = {
    memory.now = at
    val usage = Usage(Tokens(1), Tokens(1), Tokens.Zero, usd.map(BigDecimal(_)))
    val _ = memory.record(entry, turn, workflow, "m", usage, Tokens(1))(using TestTx.fake)
  }

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
}
