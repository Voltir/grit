package grit.core.claim

import grit.core.id.PluginName
import grit.core.identity.{Account, Principal, TestAccounts}
import grit.core.store.Tx
import grit.core.visibility.Clearance
import grit.dbos.sql.TestTx

/** The claims contract, kept by the in-memory fake. */
object InMemoryClaimsTests extends ClaimsContract {

  private val store = new InMemoryClaims

  protected def claims(plugin: PluginName): Claims = store.of(plugin)

  protected def person(account: Account): Principal.Person =
    TestAccounts.principal(account) match {
      case p: Principal.Person => p
      case Principal.Grit => throw new java.lang.AssertionError("grit is no person")
    }

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    body(using TestTx.fake(clearance))
}
