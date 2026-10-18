package grit.dbos.sql

import java.sql.{PreparedStatement, ResultSet}

import scala.util.Using
import scala.util.control.NonFatal

import grit.core.store.{StoreError, Tx}

/** The JDBC statement shapes the stores share, each run on the transaction's connection. A throw
  * from the database answers as [[StoreError.DatabaseError]], never as an exception.
  */
private[dbos] object SqlStatements {

  /** `body`'s value, or the database's error if it throws. */
  def attempt[A](body: => A): Either[StoreError, A] =
    try Right(body)
    catch { case NonFatal(e) => Left(SqlEntryStore.databaseError(e)) }

  /** Every row `sql`, bound by `bind`, answers, each read by `read`; or the database's error. */
  def many[A](sql: String)(bind: PreparedStatement => Unit)(read: ResultSet => A)(using
      tx: Tx^
  ): Either[StoreError, Vector[A]] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        bind(ps)
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[A]
          while (rs.next()) rows += read(rs)
          rows.result()
        }
      }
    }
  }

  /** The first row of [[many]] for the same statement, if any. */
  def one[A](sql: String)(bind: PreparedStatement => Unit)(read: ResultSet => A)(using
      tx: Tx^
  ): Either[StoreError, Option[A]] =
    many(sql)(bind)(read).map(_.headOption)

  /** Every row of `found`, or the first one's `Left` as [[StoreError.Invalid]]. */
  def valid[A](found: Vector[Either[String, A]]): Either[StoreError, Vector[A]] =
    found.foldLeft[Either[StoreError, Vector[A]]](Right(Vector.empty)) { (acc, row) =>
      acc.flatMap(done => row.map(done :+ _).left.map(StoreError.Invalid(_)))
    }

  /** The number of rows `sql`, bound by `bind`, changed; or the database's error. */
  def update(sql: String)(bind: PreparedStatement => Unit)(using
      tx: Tx^
  ): Either[StoreError, Int] = {
    val conn: java.sql.Connection^{tx} = Tx.connection(tx)
    attempt {
      Using.resource(conn.prepareStatement(sql)) { ps =>
        bind(ps)
        ps.executeUpdate()
      }
    }
  }

  /** As [[many]], for a statement beginning `WITH ${SqlClearance.With}`: the clearance's
    * parameters are set first, to the transaction's, then `bind`'s.
    */
  def cleared[A](sql: String)(bind: PreparedStatement => Unit)(read: ResultSet => A)(using
      tx: Tx^
  ): Either[StoreError, Vector[A]] = {
    val clearance = Tx.clearance(tx)
    many(sql) { ps =>
      SqlClearance.bind(ps, 1, clearance)
      bind(ps)
    }(read)
  }
}
