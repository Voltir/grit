package grit.dbos

import dev.dbos.transact.txstep.JdbcStepFactory
import grit.core.Tx

/** Runs callbacks in DBOS `txStep` transactions. */
final class Store(factory: JdbcStepFactory) {

  /** Runs `body` inside a `txStep` named `stepName`. Everything written
    * through the `Tx` capability commits atomically with the step's output
    * row, and the capability is valid only within `body`.
    */
  def transact[A: StepResult](stepName: String)(body: (Tx^) ?=> A): A =
    factory.txStep(
      new JdbcStepFactory.TransactionalFunction[A, Exception] {
        def execute(cnn: java.sql.Connection): A =
          body(using Tx.fromConnection(cnn))
      },
      stepName
    )
}
