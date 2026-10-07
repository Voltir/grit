package grit.core.identity

import grit.core.id.EdgeName

/** A person a deployment declares, by the accounts it knows them by. */
final case class DeclaredPerson(handle: Handle, accounts: Set[Account])

/** The deployment trusts `edge` to vouch for what `realm`'s source says. */
final case class Vouching(edge: EdgeName, realm: Realm)

/** Who a deployment says is whom (ADR 0032): the people it declares, each declared account
  * linked to its person and moved by nothing else, and for each realm the one edge it trusts
  * to vouch for what its source says of the accounts there ([[Standing]]).
  */
final case class Identities private (
    people: Vector[DeclaredPerson],
    vouchers: Vector[Vouching]
) {

  /** The realms `edge` vouches for; none for an edge not named. */
  def realms(edge: EdgeName): Set[Realm] = vouchers.filter(_.edge == edge).map(_.realm).toSet
}

object Identities {

  /** No one declared, no realm trusted: every account is a person of its own. */
  val Shipped: Identities = Identities(Vector.empty, Vector.empty)

  /** These, or the first mistake: an account two people declare (`AccountTwice`), a handle
    * two people take (`HandleTwice`), a person with no account (`NoAccount`), [[Account.Local]]
    * or [[Account.Grit]] declared (`Reserved`), or a realm two edges vouch for (`RealmTwice`).
    */
  def of(
      people: Vector[DeclaredPerson],
      vouchers: Vector[Vouching]
  ): Either[IdentityRefusal, Identities] =
    for {
      _ <- people.foldLeft[Either[IdentityRefusal, Map[Account, Handle]]](Right(Map.empty)) {
        (before, person) => before.flatMap(declare(_, person))
      }
      _ <- vouchers.foldLeft[Either[IdentityRefusal, Map[Realm, EdgeName]]](Right(Map.empty)) {
        (before, v) => before.flatMap(trust(_, v))
      }
    } yield Identities(people, vouchers)

  /** `person` declared after the people `by` holds, each declared account to its handle. */
  private def declare(
      by: Map[Account, Handle],
      person: DeclaredPerson
  ): Either[IdentityRefusal, Map[Account, Handle]] = {
    val handle = person.handle
    // In written order, so which of several mistakes is named does not rest on a Set's order.
    val accounts = person.accounts.toVector.sortBy(Account.written)
    if (by.valuesIterator.contains(handle)) Left(IdentityRefusal.HandleTwice(handle))
    else if (accounts.isEmpty) Left(IdentityRefusal.NoAccount(handle))
    else
      accounts
        .collectFirst {
          case a if a == Account.Local || a == Account.Grit => IdentityRefusal.Reserved(a, handle)
        }
        .orElse(
          accounts
            .flatMap(a => by.get(a).map(IdentityRefusal.AccountTwice(a, _, handle)))
            .headOption
        )
        .toLeft(by ++ accounts.map(_ -> handle))
  }

  /** `v` trusted after the realms `by` holds, each to its edge. */
  private def trust(
      by: Map[Realm, EdgeName],
      v: Vouching
  ): Either[IdentityRefusal, Map[Realm, EdgeName]] =
    by.get(v.realm) match {
      case Some(first) if first != v.edge =>
        Left(IdentityRefusal.RealmTwice(v.realm, first, v.edge))
      case _ => Right(by + (v.realm -> v.edge))
    }
}

/** Why a deployment's [[Identities]] is refused. */
enum IdentityRefusal {
  case AccountTwice(account: Account, by: Handle, and: Handle)
  case HandleTwice(handle: Handle)
  case NoAccount(handle: Handle)
  case Reserved(account: Account, by: Handle)
  case RealmTwice(realm: Realm, by: EdgeName, and: EdgeName)

  /** A line a person reads. */
  def message: String = this match {
    case AccountTwice(account, by, and) =>
      s"${Account.written(account)} is declared as both ${Handle.value(by)} and ${Handle.value(and)}"
    case HandleTwice(handle) => s"two people are declared as ${Handle.value(handle)}"
    case NoAccount(handle) => s"${Handle.value(handle)} is declared with no account"
    case Reserved(account, by) =>
      s"${Handle.value(by)} is declared holding ${Account.written(account)}, which is a principal of its own"
    case RealmTwice(realm, by, and) =>
      s"both ${EdgeName.value(by)} and ${EdgeName.value(and)} are trusted to vouch for ${realm.namespace}:${realm.within}/"
  }
}
