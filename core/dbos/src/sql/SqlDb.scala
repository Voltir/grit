package grit.dbos.sql

import java.sql.Connection
import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.store.{Db, Reads, StoreError, Tx}
import grit.core.visibility.Subject

/** [[Db]] over `dataSource`: each read is its own read-only transaction, always rolled
  * back, so nothing a body attempts to write survives, opened for its subject through
  * `opener`.
  */
final class SqlDb(dataSource: DataSource, opener: Opener) extends Db {

  def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    transaction(conn => opener.open(subject, conn))(body)

  /** Reads at maintenance's clearance, every row whatever its label: for reading a database
    * whole ([[grit.dbos.engine.Reader.all]]).
    */
  private[dbos] val all: Reads = {
    val db = this
    new Reads {
      def read[A](body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
        db.transaction(conn => Right(Tx.open(conn, opener.maintenance)))(body)
    }
  }

  private def transaction[A](open: (c: Connection^) => Either[StoreError, Tx^{c}])(
      body: (Tx^) ?=> Either[StoreError, A]
  ): Either[StoreError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        conn.setReadOnly(true)
        try open(conn).flatMap(tx => body(using tx))
        finally conn.rollback()
      }
    } catch {
      case NonFatal(e) => Left(SqlEntryStore.databaseError(e))
    }
}
