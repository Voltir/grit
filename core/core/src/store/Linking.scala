package grit.core.store

import grit.core.id.PrincipalId
import grit.core.identity.Account
import grit.core.visibility.Label

/** One change a vouching made, or why it made none. `was` and `now` are what writing through
  * the account was cleared for before and after ([[Tx.clearanceOf]]).
  * None holds an email.
  */
enum Linking {

  /** `account` is now `person`, the person of the email its realm attests. */
  case Linked(account: Account, person: PrincipalId, was: Label, now: Label)

  /** `account` is no longer `person`, an email's person. */
  case Unlinked(account: Account, person: PrincipalId, was: Label, now: Label)

  /** Its realm now attests `account` a full member, or no longer does. */
  case Standing(account: Account, member: Boolean, was: Label, now: Label)

  /** No realm the voucher holds holds `account`: nothing was recorded. */
  case Outside(account: Account)

  /** `account`'s verified email is in no domain the deployment claims: no email was kept. */
  case Unclaimed(account: Account)

  /** A line for the log, naming accounts, people's ids and clearances. */
  def message: String = this match {
    case Linked(account, person, was, now) =>
      s"${Account.written(account)} linked to ${PrincipalId.value(person)}: ${Linking.change(was, now)}"
    case Unlinked(account, person, was, now) =>
      s"${Account.written(account)} unlinked from ${PrincipalId.value(person)}: ${Linking.change(was, now)}"
    case Standing(account, member, was, now) =>
      val is = if (member) "a full member" else "not a full member"
      s"${Account.written(account)} is $is of its realm: ${Linking.change(was, now)}"
    case Outside(account) =>
      s"${Account.written(account)} not recorded: no realm the voucher holds holds it"
    case Unclaimed(account) =>
      s"${Account.written(account)}'s verified email is in no domain the deployment claims: no email kept"
  }
}

object Linking {

  private def change(was: Label, now: Label): String =
    s"${Label.written(was)} -> ${Label.written(now)}"
}
