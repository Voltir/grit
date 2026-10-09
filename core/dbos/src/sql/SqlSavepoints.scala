package grit.dbos.sql

import scala.util.control.NonFatal

import grit.core.store.{Savepoints, StoreError, Tx}

/** [[Savepoints]] as a Postgres savepoint on the transaction's connection: rolled back to on a
  * `Left`, which also clears the aborted state a failed statement leaves.
  */
object SqlSavepoints extends Savepoints {

  def atomic[A](body: Tx^ ?=> Either[StoreError, A])(using tx: Tx^): Either[StoreError, A] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    SqlEntryStore.attempt(conn.setSavepoint()).flatMap { savepoint =>
      val out =
        try body(using tx)
        catch { case NonFatal(e) => conn.rollback(savepoint); throw e }
      out match {
        case Left(_) => SqlEntryStore.attempt(conn.rollback(savepoint)).flatMap(_ => out)
        case Right(_) => SqlEntryStore.attempt(conn.releaseSavepoint(savepoint)).flatMap(_ => out)
      }
    }
  }
}
