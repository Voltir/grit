package grit.dbos.workflow

import grit.core.durable.Durable
import grit.core.id.{ShadowRef, WorkflowId}
import grit.dbos.sql.Opener

import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.workflow.QueueName
import dev.dbos.transact.workflow.{QueueConflictResolution, QueueOptions}
import dev.dbos.transact.{DBOS, DBOSClient, EnqueueOptions}

/** How a shadow is known to DBOS: the workflow it runs as, and its own queue, `shadows`, one
  * at a time across every variant, never the turns' queue, so a shadow never delays a triage
  * or a turn.
  */
object Shadows {

  /** The shadow workflow's name, as DBOS records it. */
  val WorkflowName = "shadow"

  private val Queue: QueueName = QueueName.of("shadows")

  /** Registers `body` as the shadow workflow. Must run before `dbos.launch()`. */
  private[dbos] def register(
      dbos: DBOS,
      steps: JdbcStepFactory,
      opener: Opener,
      body: WorkflowId => Durable^ ?=> String,
      running: Running
  ): Unit = {
    DurableWorkflow.register(dbos, steps, opener, WorkflowName, body, running)
    ()
  }

  /** Writes the `shadows` queue to DBOS's database as application
    * [[DurableWorkflow.ApplicationName]]'s, replacing the configuration stored there: one
    * shadow at a time, oldest first. Throws when the database fails, or when another
    * application owns the queue.
    */
  def registerQueue(client: DBOSClient): Unit =
    client.registerQueue(
      Queue.value,
      new QueueOptions().withConcurrency(1),
      QueueConflictResolution.ALWAYS_UPDATE,
      DurableWorkflow.ApplicationName
    )

  /** How `shadow` is enqueued: under its workflow id, on the `shadows` queue. */
  def enqueueOptions(shadow: ShadowRef): EnqueueOptions =
    new EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, Queue)
      .withWorkflowId(WorkflowId.value(shadow.workflowId))
}
