package grit.dbos

import grit.core.{Conversation, Origin, StoreError, Tx}
import java.sql.DriverManager
import scala.util.Using

/** Direct transactions on a live test database, for arranging rows and reading them back
  * outside the code under test.
  */
object LiveDb {

  /** Runs `body` in one transaction on `config`'s database: committed if it returns,
    * rolled back if it throws.
    */
  def transaction[A](config: DbConfig)(body: (Tx^) ?=> A): A =
    Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
      conn =>
        conn.setAutoCommit(false)
        try {
          val a = body(using Tx.fromConnection(conn))
          conn.commit()
          a
        } catch { case e: Throwable => conn.rollback(); throw e }
    }

  /** The conversation for `origin`, created if new. */
  def conversation(config: DbConfig, origin: Origin): Conversation =
    transaction(config)(new SqlConversationStore().findOrCreate(origin)) match {
      case Right(c) => c
      case Left(e: StoreError) => sys.error(s"arranging a conversation: $e")
    }

}
