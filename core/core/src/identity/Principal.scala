package grit.core.identity

import grit.core.id.PrincipalId

/** How an account is its person's. */
enum Evidence {

  /** It is its home: the person of its own it was when first seen. */
  case Home

  /** A trusted realm attests it an email in a claimed domain, so it is that email's person. */
  case Vouched
}

/** One account of a person, as the store holds it now: how it is theirs, and whether a trusted
  * realm attests it a full member.
  */
final case class Held(account: Account, evidence: Evidence, member: Boolean)

/** Who an action is done for, as the store resolves them when a transaction opens. */
enum Principal {

  /** grit itself. */
  case Grit

  /** A person, and their accounts, as the store resolved them. */
  case Person(id: PrincipalId, accounts: Set[Held])
}
