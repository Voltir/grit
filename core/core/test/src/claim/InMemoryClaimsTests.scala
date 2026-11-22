package grit.core.claim

import java.time.Instant

import grit.core.id.{ClaimHolder, PluginName, SessionId}
import grit.core.identity.{Account, Principal, TestAccounts}
import grit.core.store.Tx
import grit.core.visibility.Clearance
import grit.dbos.sql.TestTx

/** The claims contract, kept by the in-memory fake. */
object InMemoryClaimsTests extends ClaimsContract {

  private val store = new InMemoryClaims

  protected def claims(
      plugin: PluginName,
      worked: Map[ClaimHolder, Instant],
      sessions: Map[SessionId, ClaimSessions.Working]
  ): Claims = {
    val ann = person(TestAccounts.account("slack:T1/U-ann"))
    val own = worked.map((holder, at) =>
      SessionId
        .read(s"claude-code:works-${ClaimHolder.value(holder)}")
        .fold(
          e => throw new java.lang.AssertionError(e),
          _ -> ClaimSessions.Working(Some(holder), at, None, false, ann)
        )
    )
    store.of(plugin, own ++ sessions)
  }

  protected def person(account: Account): Principal.Person =
    TestAccounts.principal(account) match {
      case p: Principal.Person => p
      case Principal.Grit => throw new java.lang.AssertionError("grit is no person")
    }

  protected def opened[A](clearance: Clearance)(body: (Tx^) ?=> A): A =
    body(using TestTx.fake(clearance))
}
