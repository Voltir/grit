package grit.dbos

import grit.core.{Db, StoreError, Tx}
import javax.sql.DataSource
import scala.util.Using
import scala.util.control.NonFatal

/** [[Db]] over `dataSource`: each read is its own read-only transaction, always rolled
  * back, so nothing a body attempts to write survives.
  */
final class SqlDb(dataSource: DataSource) extends Db {

  def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        conn.setReadOnly(true)
        try body(using Tx.fromConnection(conn))
        finally conn.rollback()
      }
    } catch {
      case NonFatal(e) => Left(SqlEntryStore.databaseError(e))
    }
}
