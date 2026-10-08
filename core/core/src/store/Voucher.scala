package grit.core.store

import grit.core.identity.{Realm, Vouched}

/** The right to record what trusted realms' sources say of their accounts (ADR 0032), for the
  * realms a deployment trusts one source to answer for
  * ([[grit.core.identity.Identities.realms]]).
  */
trait Voucher extends caps.Pure {

  /** The realms whose accounts it records. */
  def realms: Set[Realm]

  /** Records what `vouched.account`'s realm says of it now, replacing what it said before:
    * whether it is a full member, and the email its source verified, kept only when the
    * deployment claims the email's domain. While that record holds an email, the account is
    * that email's person, whom every account any realm says that email for is too, made the
    * first time one does; otherwise it is the person of its own it was when first seen. An
    * account not seen before is first a new person's one account. Each change this makes to the
    * account's person or membership is one [[Linking]], none when nothing changed; an email
    * whose domain is not claimed is [[Linking.Unclaimed]]. Two vouchings of one account, or of
    * one email, wait for each other rather than deadlock. [[Linking.Outside]], recording
    * nothing, for an account no realm in [[realms]] holds.
    */
  def vouch(vouched: Vouched)(using Tx^): Either[StoreError, Vector[Linking]]
}
