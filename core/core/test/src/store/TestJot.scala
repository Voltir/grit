package grit.core.store

import grit.core.visibility.Subject
import grit.dbos.sql.TestTx

/** A [[Jot]] running each write over [[TestTx.fake]], for a suite whose stores ignore the `Tx`. */
final class TestJot extends Jot {
  def write[A](subject: Subject)(body: (Tx^) ?=> Either[StoreError, A]): Either[StoreError, A] =
    body(using TestTx.fake)
}
