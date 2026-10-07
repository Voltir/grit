package grit.dbos.engine

import javax.sql.DataSource

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.store.{StoreError, Tx}
import grit.core.visibility.Clearance
import grit.dbos.sql.SqlEntryStore

/** Short transactions over a data source, and DBOS calls, as the sweep makes them: each
  * failure a `Left`.
  */
private[engine] object Transact {

  /** `body` in a read-write transaction at `clearance`, committed on `Right`, rolled back
    * otherwise.
    */
  def write[A](
      dataSource: DataSource,
      clearance: Clearance
  )(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        val result =
          try body(using Tx.open(conn, clearance))
          catch { case NonFatal(e) => conn.rollback(); throw e }
        if (result.isRight) conn.commit() else conn.rollback()
        result
      }
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }

  /** `body` in a read-only transaction at `clearance`. */
  def read[A](
      dataSource: DataSource,
      clearance: Clearance
  )(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    try {
      Using.resource(dataSource.getConnection()) { conn =>
        conn.setAutoCommit(false)
        conn.setReadOnly(true)
        try body(using Tx.open(conn, clearance))
        finally conn.rollback()
      }
    } catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }

  /** `body`, a DBOS call, with what it throws as a `Left`. */
  def attempted[A](body: => A): Either[StoreError, A] =
    try Right(body)
    catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }
}
