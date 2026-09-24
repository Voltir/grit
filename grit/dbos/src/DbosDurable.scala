package grit.dbos

import dev.dbos.transact.DBOS
import dev.dbos.transact.execution.ThrowingSupplier
import dev.dbos.transact.txstep.JdbcStepFactory
import grit.core.{Durable, Journaled, Tx, UnreadableJournal, WorkflowId}

/** [[Durable]] over DBOS, for the workflow `workflowId`: `step` is a DBOS step, `transact`
  * a `txStep`. Outputs cross DBOS as the `String` their [[Journaled]] encodes to, so its
  * serializer never sees a grit type. Steps run once, with no retries. `patch` needs
  * patching enabled in DBOS's config ([[Engine]] does).
  */
private[dbos] final class DbosDurable(
    dbos: DBOS,
    steps: JdbcStepFactory,
    workflowId: WorkflowId
) extends Durable {

  def step[A: Journaled](name: String)(body: () => A): A =
    decode(
      name,
      dbos.runStep(
        new ThrowingSupplier[String, Exception] {
          def execute(): String = summon[Journaled[A]].encode(body())
        },
        name
      )
    )

  def transact[A: Journaled](name: String)(body: (Tx^) ?=> A): A =
    decode(
      name,
      steps.txStep(
        new JdbcStepFactory.TransactionalFunction[String, Exception] {
          def execute(cnn: java.sql.Connection): String =
            summon[Journaled[A]].encode(body(using Tx.fromConnection(cnn)))
        },
        name
      )
    )

  def patch(name: String): Boolean = dbos.patch(name)

  def deprecatePatch(name: String): Unit = {
    dbos.deprecatePatch(name)
    ()
  }

  // A fresh output is decoded too, so a first run returns exactly what a replay would.
  private def decode[A](name: String, recorded: String)(using j: Journaled[A]): A =
    j.decode(recorded) match {
      case Right(a) => a
      case Left(reason) => throw UnreadableJournal(workflowId, name, reason)
    }
}
