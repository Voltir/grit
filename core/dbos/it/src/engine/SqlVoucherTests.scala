package grit.dbos.engine

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.FiniteDuration

import grit.core.identity.Account
import grit.core.store.{Tx, Voucher, VoucherContract}
import grit.dbos.sql.{DbConfig, LiveDb, TestPostgres}

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

  protected def aged(voucher: Voucher, account: Account, ago: FiniteDuration): Unit =
    Vouchings.aged(account, s"${ago.toMillis} milliseconds")(using config)

  protected def transaction[A](body: (Tx^) ?=> A): A = LiveDb.transaction(config)(body)
}
