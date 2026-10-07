package grit.core.store

import grit.core.id.{PrincipalId, ShortHash}
import grit.core.identity.{Account, Email}
import grit.core.visibility.Label

/** One change a vouching made, or why it made none. `was` and `now` are what writing through
  * the account was cleared for before and after ([[grit.core.visibility.Visibility.cleared]]).
  */
enum Linking {
  case Linked(account: Account, person: PrincipalId, was: Label, now: Label)
  case Unlinked(account: Account, person: PrincipalId, was: Label, now: Label)

  /** Its realm now vouches `account` a full member, or no longer does. */
  case Standing(account: Account, member: Boolean, was: Label, now: Label)
  case Refused(account: Account, why: LinkRefusal)

  /** A line for the log, naming accounts and clearances, never an address: an email account is
    * written `email:#{hash}`, a short hash of its address.
    */
  def message: String = this match {
    case Linked(account, person, was, now) =>
      s"${Linking.shown(account)} linked to ${PrincipalId.value(person)}: ${Linking.change(was, now)}"
    case Unlinked(account, person, was, now) =>
      s"${Linking.shown(account)} unlinked from ${PrincipalId.value(person)}: ${Linking.change(was, now)}"
    case Standing(account, member, was, now) =>
      val is = if (member) "a full member" else "not a full member"
      s"${Linking.shown(account)} is $is of its realm: ${Linking.change(was, now)}"
    case Refused(account, why) =>
      val because = why match {
        case LinkRefusal.OutsideRealms => "no realm vouched for holds it"
        case LinkRefusal.Declared => "the deployment declares it"
        case LinkRefusal.Joins(other) =>
          s"its email is ${PrincipalId.value(other)}'s, and it is not alone in its own person"
      }
      s"${Linking.shown(account)} not linked: $because"
  }
}

object Linking {

  private def shown(account: Account): String =
    Account
      .address(account)
      .fold(Account.written(account))(e => s"email:#${ShortHash.of(Email.value(e))}")

  private def change(was: Label, now: Label): String =
    s"${Label.written(was)} -> ${Label.written(now)}"
}

/** Why a vouching changed nothing. */
enum LinkRefusal {

  /** No realm the voucher holds holds the account. */
  case OutsideRealms

  /** The deployment declares the account; only its declaration moves it. */
  case Declared

  /** Another person, `other`, holds the vouched email's account, and the account is not alone
    * in its own: two people are joined only by a declaration.
    */
  case Joins(other: PrincipalId)
}
