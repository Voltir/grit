package grit.core.claim

import java.time.Instant

import grit.core.id.{ClaimHolder, PluginName}
import grit.core.identity.{Account, Principal, TestAccounts}
import grit.core.store.Tx
import grit.core.visibility.Clearance
import grit.dbos.sql.TestTx

/** The claims contract, kept by the in-memory fake. */
object InMemoryClaimsTests extends ClaimsContract {

  private val store = new InMemoryClaims

  protected def claims(plugin: PluginName, worked: Map[ClaimHolder, Instant]): Claims =
    store.of(plugin, worked.get)

  protected def person(account: Account): Principal.Person =
    TestAccounts.principal(account) match {
      case p: Principal.Person => p
      case Principal.Grit => throw new java.lang.AssertionError("grit is no person")
    }

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    body(using TestTx.fake(clearance))
}
