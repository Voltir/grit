package grit.dbos.sql

import grit.core.id.PrincipalId
import grit.core.identity.Account
import grit.core.store.StoreError

/** Accounts in the columns that name a principal (`inbound.author`, `conversations.created_by`,
  * a review's rater, a heard message's reach): until the store keeps identities, each principal
  * is a person of one account, and its id is that account's spelling.
  */
private[dbos] object SqlAccounts {

  /** `account` as a principal column holds it. */
  def written(account: Account): String = Account.written(account)

  /** The account a principal column's `text` holds; `Invalid` when it is no account's
    * spelling.
    */
  def read(text: String): Either[StoreError, Account] =
    Account.read(text).left.map(why => StoreError.Invalid(s"a stored account: $why"))

  /** The principal whose one account is `account`. */
  def principal(account: Account): PrincipalId = PrincipalId(Account.written(account))
}
