package grit.dbos.sql

import grit.core.store.Tx
import grit.core.visibility.{Clearance, Label, Recorded, Visibility}

/** Test fixture: a fake [[Tx]] whose backing Connection is never touched.
  * The `null` lives here so that STYLE rule 6 holds everywhere else.
  *
  * It sits in `core`'s test tree because `core.test` cannot depend on the `dbos`
  * module; rule 6 is scoped by package, so the package is what keeps it legal.
  */
object TestTx {

  /** One opened at `clearance`, under `visibility` and what `recorded` holds. */
  def fake(
      clearance: Clearance,
      visibility: Visibility = Visibility.Shipped,
      recorded: Recorded = Recorded.Empty
  ): Tx =
    Tx.open(null, clearance, visibility, recorded)

  /** One whose labels in force are `visibility`'s and `recorded`'s, opened at public: for a
    * fake reading a room's label or a person's clearance as a real transaction would.
    */
  def inForce(visibility: Visibility, recorded: Recorded = Recorded.Empty): Tx =
    fake(Clearance.of(Label.Public), visibility, recorded)

  /** One opened at the public clearance, which every label of the shipped visibility is under. */
  def fake: Tx = fake(Clearance.of(Label.Public))
}
