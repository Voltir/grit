package grit.core.identity

import grit.core.id.PrincipalId

/** Accounts as the suites spell them, and the principal core's in-memory stores take one to
  * be, as `grit.dbos` does while a principal's id is its one account's spelling.
  */
object TestAccounts {

  /** `text` as an account ([[Account.read]]), failing the test when it is none. */
  def account(text: String): Account =
    Account.read(text).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The principal whose one account is `account`: its id is the account's spelling. */
  def principalId(account: Account): PrincipalId = PrincipalId(Account.written(account))
}
