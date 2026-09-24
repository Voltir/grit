package grit.dbos.sql

import java.sql.DriverManager

import scala.util.Using

import grit.core.store.{Conversation, Origin, StoreError, Tx}

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

  /** Every ledger row: entry, model, cost, and the estimate of the request's input. */
  def ledger(config: DbConfig): Vector[(String, String, Option[BigDecimal], Long)] =
    transaction(config) { (tx: Tx^) ?=>
      val conn: java.sql.Connection^{tx} = Tx.connection(tx)
      Using.resource(
        conn.prepareStatement(
          "SELECT entry_id, model, cost_usd, estimated_input_tokens FROM grit.usage_ledger ORDER BY entry_id"
        )
      ) { ps =>
        Using.resource(ps.executeQuery()) { rs =>
          val rows = Vector.newBuilder[(String, String, Option[BigDecimal], Long)]
          while (rs.next())
            rows += ((
              rs.getString(1),
              rs.getString(2),
              Option(rs.getBigDecimal(3)).map(BigDecimal(_)),
              rs.getLong(4)
            ))
          rows.result()
        }
      }
    }
}
