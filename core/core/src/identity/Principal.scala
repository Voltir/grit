package grit.core.identity

import grit.core.id.PrincipalId

/** How an account is its person's. */
enum Evidence {

  /** Seen first, it is a person of its own. */
  case Enrolled

  /** The deployment declares it the person's ([[Identities]]). */
  case Declared

  /** A trusted realm vouched an email that links it, or that it is. */
  case Vouched
}

/** One account of a person, as the store holds it now: how it is theirs, and whether a trusted
  * realm vouches it a full member.
  */
final case class Held(account: Account, evidence: Evidence, member: Boolean)

/** Who an action is done for, as the store resolves them when a transaction opens. */
enum Principal {

  /** grit itself. */
  case Grit

  /** A person, with the deployment's handle for them when it declares them, and their
    * accounts, as the store resolved them.
    */
  case Person(id: PrincipalId, handle: Option[Handle], accounts: Set[Held])
}
