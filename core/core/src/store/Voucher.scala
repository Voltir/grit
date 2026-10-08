package grit.core.store

import scala.concurrent.duration.FiniteDuration

import grit.core.identity.{Account, Realm, Vouched}

/** The right to record what trusted realms' sources say of their accounts (ADR 0032), for the
  * realms a deployment trusts one attester to answer for
  * ([[grit.core.identity.Identities.realmsOf]]).
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
    * account's person or membership is one [[Linking]], with [[Linking.Unclaimed]] beside them
    * when the email it did not keep is in no claimed domain; none when nothing changed, as when
    * the same answer is recorded again. Two vouchings of one account, or of one email, wait for
    * each other rather than deadlock; two transactions each vouching several accounts can
    * deadlock, so each vouching is a transaction of its own
    * ([[grit.core.edge.Attesting]], its one holder, records each answer so). [[Linking.Outside]], recording nothing, for an account no
    * realm in [[realms]] holds.
    */
  def vouch(vouched: Vouched)(using Tx^): Either[StoreError, Vector[Linking]]

  /** How long ago `account`'s realm last said anything of it. */
  def lastWord(account: Account)(using Tx^): Either[StoreError, LastWord]

  /** The same of every account of `realm` seen so far, in no order. */
  def lastWords(realm: Realm)(using Tx^): Either[StoreError, Vector[LastWord]]
}

/** How long ago `account`'s realm last said anything of it: `None` when it never has. */
final case class LastWord(account: Account, ago: Option[FiniteDuration])
