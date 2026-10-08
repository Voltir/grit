package grit.dbos.engine

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.FiniteDuration
import scala.util.Using

import grit.core.identity.Account
import grit.core.store.{Tx, Voucher, VoucherContract}
import grit.core.visibility.GroupName
import grit.dbos.sql.{DbConfig, LiveDb, SqlEntryStore, SqlIdentities, TestPostgres}

/** The voucher contract, kept by the engine's voucher against a real Postgres. */
object SqlVoucherTests extends VoucherContract {
  import VoucherContract.*

  // Each test gets a database of its own, so no test sees another's people. Only ever holds an
  // immutable config; the suite's tests run one at a time.
  @caps.unsafe.untrackedCaptures
  private var database: Option[DbConfig] = None

  private val made = new AtomicInteger

  private def config: DbConfig =
    database.getOrElse(throw new java.lang.AssertionError("no database"))

  protected def fresh(): Voucher = {
    val c = TestPostgres.freshDatabase(s"sql_voucher_${made.incrementAndGet()}")
    val engine = LiveEngine.open(c, "test", visibility = Cleared)
    database = Some(c)
    try engine.voucher(Set(T1, T2), Claimed)
    finally engine.close()
  }

  protected def none(): Voucher = {
    val c = TestPostgres.freshDatabase(s"sql_voucher_${made.incrementAndGet()}")
    val engine = LiveEngine.open(c, "test", visibility = Cleared)
    database = Some(c)
    try engine.voucher(Set.empty, Claimed)
    finally engine.close()
  }

  protected def saw(voucher: Voucher, account: Account): Unit =
    Vouchings.enrolled(account)(using config)

  protected def added(voucher: Voucher, account: Account, group: GroupName): Unit =
    LiveDb
      .transaction(config) { (tx: Tx^) ?=>
        SqlIdentities.enroll(Set(account)).flatMap { _ =>
          SqlEntryStore.attempt {
            val conn: java.sql.Connection^{tx} = Tx.connection(tx)
            Using.resource(
              conn.prepareStatement(
                "INSERT INTO grit.group_members (group_name, account) VALUES (?, ?)"
              )
            ) { ps =>
              ps.setString(1, GroupName.value(group))
              ps.setString(2, Account.written(account))
              ps.executeUpdate()
            }
          }
        }
      }
      .fold(e => throw new java.lang.AssertionError(s"adding: $e"), _ => ())

  protected def aged(voucher: Voucher, account: Account, ago: FiniteDuration): Unit =
    Vouchings.aged(account, s"${ago.toMillis} milliseconds")(using config)

  /* Under the engine's visibility, as the engine's own transactions are: the voucher says each
   * change's clearances as the transaction it runs in clears people. */
  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.under(config, Cleared)(body)
}
