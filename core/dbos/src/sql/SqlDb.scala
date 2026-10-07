package grit.dbos.sql

import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.store.{Db, StoreError, Tx}

/** [[Db]] over `dataSource`: each read is its own read-only transaction, always rolled
  * back, so nothing a body attempts to write survives.
  */
final class SqlDb(dataSource: DataSource, opener: Opener) extends Db {

  def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        conn.setReadOnly(true)
        try body(using Tx.open(conn, opener.maintenance))
        finally conn.rollback()
      }
    } catch {
      case NonFatal(e) => Left(SqlEntryStore.databaseError(e))
    }
}
