package grit.core.store

import scala.concurrent.duration.FiniteDuration

import grit.core.identity.Account
import grit.dbos.sql.TestTx

/** The voucher contract, kept by the in-memory fake. */
object InMemoryVoucherTests extends VoucherContract {
  import VoucherContract.*

  protected def fresh(): Voucher = new InMemoryVoucher(Set(T1, T2), Claimed, Cleared)

  private def fake(voucher: Voucher): InMemoryVoucher = voucher match {
    case v: InMemoryVoucher => v
    case _ => throw new java.lang.AssertionError("not the in-memory voucher")
  }

  protected def saw(voucher: Voucher, account: Account): Unit = fake(voucher).saw(account)

  protected def aged(voucher: Voucher, account: Account, ago: FiniteDuration): Unit =
    fake(voucher).aged(account, ago)

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
}
