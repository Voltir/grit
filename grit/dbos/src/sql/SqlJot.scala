package grit.dbos.sql

import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.store.{Jot, StoreError, Tx}

/** [[Jot]] over `dataSource`: each write is its own transaction, committed when its body
  * returns a `Right` and rolled back otherwise.
  */
final class SqlJot(dataSource: DataSource) extends Jot {

  def write[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        val result =
          try body(using Tx.fromConnection(conn))
          catch {
            case NonFatal(e) =>
              conn.rollback()
              throw e
          }
        result match {
          case Right(_) => conn.commit()
          case Left(_) => conn.rollback()
        }
        result
      }
    } catch {
      case NonFatal(e) => Left(SqlEntryStore.databaseError(e))
    }
}
