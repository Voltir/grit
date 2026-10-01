package grit.dbos.workflow

import scala.concurrent.duration.FiniteDuration
import scala.jdk.OptionConverters.*

import grit.core.durable.{Durable, Journaled, StreamWriter, UnreadableJournal}
import grit.core.id.WorkflowId
import grit.core.store.Tx

import dev.dbos.transact.DBOS
import dev.dbos.transact.execution.ThrowingSupplier
import dev.dbos.transact.txstep.JdbcStepFactory

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

  /** `DBOS.writeStream` under `key`: from inside a step, a row in `dbos.streams` under the
    * step's own function id, so no operation is recorded (DBOSExecutor.writeStream).
    */
  def stream(key: String): StreamWriter = {
    val on = dbos
    new StreamWriter {
      def write(piece: String): Unit = on.writeStream(key, piece)
    }
  }

  /** `DBOS.recv`: two operations, `DBOS.recv` recording the message (or null) and, under the
    * next function id, `DBOS.sleep` recording the wait's end, written first
    * (NotificationsDAO.recv, StepsDAO.durableSleepEndTime). A message is whatever its sender
    * sent, deserialized; one that is not a string is its `toString`.
    */
  def recv(topic: String, timeout: FiniteDuration): Option[String] =
    dbos
      .recv[AnyRef](topic, java.time.Duration.ofMillis(timeout.toMillis))
      .toScala
      .map {
        case s: String => s
        case other => other.toString
      }

  // A fresh output is decoded too, so a first run returns exactly what a replay would.
  private def decode[A](name: String, recorded: String)(using j: Journaled[A]): A =
    j.decode(recorded) match {
      case Right(a) => a
      case Left(reason) => throw UnreadableJournal(workflowId, name, reason)
    }
}
