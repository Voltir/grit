package grit.core.store

import grit.core.visibility.{Clearance, Label, Subject, Visibility}
import grit.dbos.sql.TestTx

/** A [[Jot]] running each write over [[TestTx.fake]], never rolled back, for in-memory stores:
  * each write at what `clearance` says then, under `visibility`.
  */
final class TestJot(
    clearance: () -> Clearance = () => Clearance.of(Label.Public),
    visibility: Visibility = Visibility.Shipped
) extends Jot {
  def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    body(using TestTx.fake(clearance(), visibility))
}
