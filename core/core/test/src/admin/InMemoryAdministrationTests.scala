package grit.core.admin

import grit.core.identity.Account
import grit.core.place.Place
import grit.core.store.InMemoryVoucher
import grit.core.visibility.RoomAccess

/** The administration contract, kept by the in-memory fake. */
object InMemoryAdministrationTests extends AdministrationContract {
  import AdministrationContract.*

  protected def fresh(): Administration =
    new InMemoryAdministration(Declared, new InMemoryVoucher(Set(T1), Set.empty, Declared))

  private def fake(a: Administration): InMemoryAdministration = a match {
    case f: InMemoryAdministration => f
    case _ => throw new java.lang.AssertionError("not the in-memory administration")
  }

  protected def member(a: Administration, account: Account): Unit = fake(a).member(account)

  protected def seen(a: Administration, account: Account): Unit = fake(a).voucher.saw(account)

  protected def reported(a: Administration, room: Place, access: RoomAccess): Unit =
    fake(a).reported(room, access)

  protected def kept(a: Administration): Vector[Kept] = fake(a).kept
}
