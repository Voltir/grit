package grit.dbos.workflow

import grit.core.durable.Durable
import grit.core.id.WorkflowId
import grit.core.stitch.Opening

import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.workflow.QueueName
import dev.dbos.transact.workflow.{QueueConflictResolution, QueueOptions}
import dev.dbos.transact.{DBOS, DBOSClient, EnqueueOptions}

/** How an opening's placement is known to DBOS (ADR 0023): the workflow it runs as, and its
  * own queue, partitioned by room, so a room's openings are placed one at a time, in the order
  * queued, and never hold up a turn.
  */
object Stitches {

  private val WorkflowName = "stitch"

  private val Queue: QueueName = QueueName.of("stitches")

  /** Registers `body` as the placement workflow. Must run before `dbos.launch()`. */
  def register(
      dbos: DBOS,
      steps: JdbcStepFactory,
      body: WorkflowId => Durable^ ?=> String,
      running: Running
  ): Unit = {
    DurableWorkflow.register(dbos, steps, WorkflowName, body, running)
    ()
  }

  /** Writes the `stitches` queue to DBOS's database as application
    * [[DurableWorkflow.ApplicationName]]'s, replacing the configuration stored there: within
    * each partition (a room) one placement at a time, oldest first, ties in workflow id order,
    * and no limit across partitions. Throws when the database fails, or when another
    * application owns the queue.
    */
  def registerQueue(client: DBOSClient): Unit =
    client.registerQueue(
      Queue.value,
      new QueueOptions().withPartitionConcurrency(1),
      QueueConflictResolution.ALWAYS_UPDATE,
      DurableWorkflow.ApplicationName
    )

  /** How `opening`'s placement is enqueued: under its workflow id, on the stitches queue,
    * partitioned by its room.
    */
  def enqueueOptions(opening: Opening): EnqueueOptions =
    new EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, Queue)
      .withWorkflowId(WorkflowId.value(opening.ref.workflowId))
      .withQueuePartitionKey(opening.room.written)
}
