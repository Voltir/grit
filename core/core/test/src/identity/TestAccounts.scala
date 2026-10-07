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

  /** Whom writing through `account` is done for, as `grit.dbos` resolves it: grit for
    * [[Account.Grit]]; otherwise the person of that one account, enrolled, whom no realm vouches
    * a full member.
    */
  def principal(account: Account): Principal =
    if (account == Account.Grit) Principal.Grit
    else
      Principal.Person(
        principalId(account),
        None,
        Set(Held(account, Evidence.Enrolled, member = false))
      )
}
