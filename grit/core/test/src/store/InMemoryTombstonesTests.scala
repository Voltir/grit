package grit.core.store

import grit.dbos.sql.TestTx

/** The tombstones contract, kept by the in-memory fake. */
object InMemoryTombstonesTests extends TombstonesContract {
  protected val tombstones: Tombstones = new InMemoryTombstones
  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
}
