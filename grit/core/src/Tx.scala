package grit.core

/** Capability to read and write inside the current database transaction.
  * Obtained from [[grit.dbos.Store.transact]]; a `Tx` is valid only for
  * the duration of that callback, and the type system rejects any attempt to
  * keep it beyond it.
  */
opaque type Tx = java.sql.Connection

object Tx {
  private[grit] def fromConnection(c: java.sql.Connection^): Tx^{c} = c
  private[grit] def connection(tx: Tx^): java.sql.Connection^{tx} = tx
}
