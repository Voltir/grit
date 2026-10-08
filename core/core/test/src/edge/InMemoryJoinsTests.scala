package grit.core.edge

import grit.core.identity.Account
import grit.core.place.Place
import grit.core.store.InMemoryVoucher
import grit.core.visibility.{RoomAccess, Visibility}

/** The joins contract, kept by the in-memory fake. */
object InMemoryJoinsTests extends JoinsContract {
  import JoinsContract.*

  protected def fresh(): Joins =
    new InMemoryJoins(new InMemoryVoucher(Set(T1), Set.empty, Visibility.Shipped))

  private def fake(j: Joins): InMemoryJoins = j match {
    case f: InMemoryJoins => f
    case _ => throw new java.lang.AssertionError("not the in-memory joins")
  }

  protected def member(j: Joins, account: Account): Unit = fake(j).member(account)

  protected def decide(j: Joins, room: Place): Unit = fake(j).decide(room)

  protected def access(j: Joins, room: Place): Option[RoomAccess] = fake(j).access(room)
}
