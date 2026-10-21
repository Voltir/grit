package grit.core.store

import grit.core.id.{EntryId, Href}
import grit.core.identity.Account
import grit.dbos.sql.TestTx

/** The principals contract, kept by the in-memory fake. */
object InMemoryPrincipalsTests extends PrincipalsContract {

  // A counter only the contract's own calls read, on the test's one thread.
  @caps.unsafe.untrackedCaptures
  private var n = 0

  protected def fresh(): Principals = new InMemoryPrincipals

  protected def said(principals: Principals, by: Account, link: Option[Href]): EntryId = {
    n += 1
    val id = EntryId(s"in:c:$n")
    principals match {
      case p: InMemoryPrincipals => p.authored(id, by, link)
      case _ => throw new java.lang.AssertionError("not the in-memory principals")
    }
    id
  }

  protected def transaction[A](body: (Tx^) ?=> A): A = body(using TestTx.fake)
}
