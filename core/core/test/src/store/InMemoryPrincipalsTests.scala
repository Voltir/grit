package grit.core.store

import grit.core.id.EntryId
import grit.core.identity.Account
import grit.dbos.sql.TestTx

/** The principals contract, kept by the in-memory fake. */
object InMemoryPrincipalsTests extends PrincipalsContract {

  @caps.unsafe.untrackedCaptures
  private var n = 0

  protected def fresh(): Principals = new InMemoryPrincipals

  protected def said(principals: Principals, by: Account): EntryId = {
    n += 1
    val id = EntryId(s"in:c:$n")
    principals match {
      case p: InMemoryPrincipals => p.authored(id, by)
      case _ => throw new java.lang.AssertionError("not the in-memory principals")
    }
    id
  }

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
}
