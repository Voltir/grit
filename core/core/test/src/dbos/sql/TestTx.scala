package grit.dbos.sql

import grit.core.store.Tx
import grit.core.visibility.{Clearance, Label, Visibility}

/** Test fixture: a fake [[Tx]] whose backing Connection is never touched.
  * The `null` lives here so that STYLE rule 6 holds everywhere else.
  *
  * It sits in `core`'s test tree because `core.test` cannot depend on the `dbos`
  * module; rule 6 is scoped by package, so the package is what keeps it legal.
  */
object TestTx {

  /** One opened at `clearance`, labelling places as `visibility` does. */
  def fake(clearance: Clearance, visibility: Visibility = Visibility.Shipped): Tx =
    Tx.open(null, clearance, visibility)

  /** One opened at the public clearance, which every label of the shipped visibility is under. */
  def fake: Tx = fake(Clearance.of(Label.Public))
}
