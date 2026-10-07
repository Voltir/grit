package grit.dbos.sql

import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.store.{Jot, StoreError, Tx}
import grit.core.visibility.Subject

/** [[Jot]] over `dataSource`: each write is its own transaction, committed when its body
  * returns a `Right` and rolled back otherwise.
  */
final class SqlJot(dataSource: DataSource, opener: Opener) extends Jot {

  def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        val result =
          try opener.open(subject, conn).flatMap(tx => body(using tx))
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
