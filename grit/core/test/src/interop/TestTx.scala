package grit.core.interop

import grit.core.Tx

/** Test fixture: a fake [[Tx]] whose backing Connection is never touched.
  * The `null` lives here so that STYLE rule 6 holds everywhere else.
  */
object TestTx {
  def fake: Tx = Tx.fromConnection(null)
}
