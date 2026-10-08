package grit.core.edge

import grit.core.admin.{Administration, InMemoryAdministration}
import grit.core.admin.AdministrationContract.{Declared, T1, ada, mia}
import grit.core.inbox.{Inbox, InMemoryInbox}
import grit.core.store.{InMemoryVoucher, Origin}
import grit.core.visibility.Label

/** The recorded contract, kept by the in-memory fakes over one voucher's record. */
object InMemoryRecordedTests extends RecordedContract {

  protected def withStores[A](
      body: (Administration, Joins, Inbox, Origin => Option[Label]) => A
  ): A = {
    val voucher = new InMemoryVoucher(Set(T1), Set.empty, Declared)
    val admin = new InMemoryAdministration(Declared, voucher)
    Vector(mia, ada).foreach(admin.member)
    val inbox = InMemoryInbox.fresh(
      visibility = Declared,
      person = voucher.principal,
      records = voucher.records
    )
    body(
      admin,
      new InMemoryJoins(voucher),
      inbox,
      origin => inbox.conversations.all.find(_.origin == origin).map(_.label)
    )
  }
}
