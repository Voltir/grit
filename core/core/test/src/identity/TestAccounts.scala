package grit.core.identity

import grit.core.id.{PrincipalId, TestPrincipalIds}

/** Accounts as the suites spell them, and the principal core's in-memory stores take one to
  * be: a person of that one account, whose id is the account's spelling. `grit.dbos` mints a
  * person's id instead, so a contract run against both asks its store whom an account is linked
  * to, never this.
  */
object TestAccounts {

  /** `text` as an account ([[Account.read]]), failing the test when it is none. */
  def account(text: String): Account =
    Account.read(text).fold(e => throw new java.lang.AssertionError(e), identity)

  /** The principal whose one account is `account`: its id is the account's spelling. */
  def principalId(account: Account): PrincipalId =
    TestPrincipalIds.stored(Account.written(account))

  /** Whom writing through `account` is done for, as `grit.dbos` resolves an account seen once
    * and never declared or vouched: grit for [[Account.Grit]]; otherwise the person of that one
    * account, enrolled, whom no realm vouches a full member.
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
