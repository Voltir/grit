package grit.eval.harness.sent

import grit.core.store.StoreError

/** How the harness says a read through [[grit.dbos.internal.Reader]] failed. */
object Unread {

  /** `<what> unread: <the failure's kind>`. */
  def said(what: String)(e: StoreError): String = s"$what unread: ${e.kind}"
}
