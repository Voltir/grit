package grit.core.store

import grit.core.identity.{Realm, Vouched}

/** The right to say what a trusted realm's source says of its accounts (ADR 0032), held only by
  * the edge a deployment names for [[realms]] ([[grit.core.identity.Identities.realms]]).
  */
trait Voucher extends caps.Pure {

  def realms: Set[Realm]

  /** Keeps `vouched.standing` as what its account's source says now: whether it is a full
    * member, and the email verified for it; then relinks by the email. An account an earlier
    * vouching linked, whose email is now another or none, first goes back to the person of its
    * own it was when first seen. Then, when an email is vouched and its account is not already
    * the account's person's: held by no one, it is created in the account's person; held alone
    * and only enrolled, it moves there; held by anyone else, the account moves to them if it is
    * its person's only account and only enrolled. Every link an earlier vouching made that this
    * one no longer supports goes back the same way. A person left with no account by a move is
    * kept, for the account to go back to. Each change is one [[Linking]], none when nothing
    * changed. An account outside [[realms]] is [[Linking.Refused]] and nothing changes; one the
    * deployment declares keeps its standing and is never moved (`Refused(Declared)` when its
    * email would have moved it); one that is not alone while another person holds its email is
    * `Refused(Joins)`, its standing kept.
    */
  def vouch(vouched: Vouched)(using Tx^): Either[StoreError, Vector[Linking]]
}
