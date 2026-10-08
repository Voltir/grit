package grit.core.identity

import grit.core.id.AttesterName

/** The deployment trusts `attester` to say what `realm`'s source says of its accounts. What it
  * says stands until it says otherwise or the deployment stops trusting it; grit asks again
  * when its rules call for it.
  */
final case class Vouching(attester: AttesterName, realm: Realm)

/** How a deployment identifies people (ADR 0032): for each realm it trusts, the one attester
  * that answers for it; and the email domains it claims as its own, whose addresses alone link
  * a trusted realm's accounts into one person. With none claimed, nothing links by email, and a
  * realm's word that an account is a full member still counts.
  */
final case class Identities private (vouchings: Vector[Vouching], domains: Set[Domain]) {

  /** The realms `attester` answers for; none for an attester not named. */
  def realmsOf(attester: AttesterName): Set[Realm] =
    vouchings.filter(_.attester == attester).map(_.realm).toSet

  /** Every realm trusted. */
  def realms: Set[Realm] = vouchings.map(_.realm).toSet

  /** Every attester named. */
  def attesters: Set[AttesterName] = vouchings.map(_.attester).toSet
}

object Identities {

  /** No realm trusted and no domain claimed: every account is a person of its own. */
  val Shipped: Identities = Identities(Vector.empty, Set.empty)

  /** These, or `RealmTwice` for a realm two attesters vouch for. */
  def of(vouchings: Vector[Vouching], domains: Set[Domain]): Either[IdentityRefusal, Identities] =
    vouchings
      .foldLeft[Either[IdentityRefusal, Map[Realm, AttesterName]]](Right(Map.empty)) {
        (before, v) => before.flatMap(trust(_, v))
      }
      .map(_ => Identities(vouchings, domains))

  /** `v` trusted after the realms `by` holds, each to its attester. */
  private def trust(
      by: Map[Realm, AttesterName],
      v: Vouching
  ): Either[IdentityRefusal, Map[Realm, AttesterName]] =
    by.get(v.realm) match {
      case Some(first) if first != v.attester =>
        Left(IdentityRefusal.RealmTwice(v.realm, first, v.attester))
      case _ => Right(by + (v.realm -> v.attester))
    }
}

/** Why a deployment's [[Identities]] is refused. */
enum IdentityRefusal {
  case RealmTwice(realm: Realm, by: AttesterName, and: AttesterName)

  /** A line a person reads. */
  def message: String = this match {
    case RealmTwice(realm, by, and) =>
      s"both ${AttesterName.value(by)} and ${AttesterName.value(and)} are trusted to attest ${realm.namespace}:${realm.within}/"
  }
}
