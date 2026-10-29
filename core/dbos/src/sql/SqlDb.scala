package grit.dbos.sql

import java.sql.Connection
import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.store.{Db, StoreError, Tx}
import grit.core.visibility.Subject

/** [[Db]] over `dataSource`: each read is its own read-only transaction, always rolled
  * back, so nothing a body attempts to write survives, opened for its subject through
  * `opener`.
  */
final class SqlDb(dataSource: DataSource, opener: Opener) extends Db {

  def read[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    transaction(conn => opener.open(subject, conn))(body)

  /** As [[read]], its transaction opened by `open`: for this module's own reads, which open at
    * maintenance's clearance ([[grit.dbos.internal.Reader.all]]).
    */
  private[dbos] def transaction[A](open: (c: Connection^) => Either[StoreError, Tx^{c}])(
      body: (Tx^) ?=> Either[StoreError, A]
  ): Either[StoreError, A] =
    SqlDb.rolledBack(dataSource)(conn => open(conn).flatMap(tx => body(using tx)))(identity)
}

private[dbos] object SqlDb {

  /** `step` on a connection of `dataSource`, read-only and always rolled back; a throw, as
    * `failed` makes it of the database error it is.
    */
  def rolledBack[E, A](dataSource: DataSource)(step: Connection => Either[E, A])(
      failed: StoreError.DatabaseError => E
  ): Either[E, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        conn.setReadOnly(true)
        try step(conn)
        finally conn.rollback()
      }
    } catch { case NonFatal(e) => Left(failed(SqlEntryStore.databaseError(e))) }
}
