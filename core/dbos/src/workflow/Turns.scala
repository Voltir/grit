package grit.dbos.workflow

import grit.core.durable.Durable
import grit.core.id.{ConversationId, TurnRef, WorkflowId}

import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.workflow.{QueueConflictResolution, QueueOptions}
import dev.dbos.transact.{DBOS, DBOSClient}

/** How a turn is known to DBOS: the workflow it runs as, and the queue that runs one turn
  * per conversation at a time, oldest first. DBOS counts running workflows and dequeues
  * per partition key, and the key is the conversation id.
  *
  * The engine registers and an edge enqueues through this one object, so the two cannot
  * name the workflow differently. A mismatch would enqueue fine and fail only on dequeue.
  */
object Turns {

  private val WorkflowName = "turn"

  /** The queue turns and closes share, partitioned by conversation, so a close never runs
    * beside a turn of its conversation ([[Closes]]).
    */
  private[workflow] val QueueName = "turns"

  /** Registers `body` as the turn workflow. Must run before `dbos.launch()`. */
  def register(
      dbos: DBOS,
      steps: JdbcStepFactory,
      body: WorkflowId => Durable^ ?=> String,
      running: Running
  ): Unit = {
    DurableWorkflow.register(dbos, steps, WorkflowName, body, running)
    ()
  }

  /** Writes the `turns` queue to DBOS's database as application
    * [[DurableWorkflow.ApplicationName]]'s, replacing the configuration stored there: within
    * each partition one workflow at a time, oldest first, and no limit across partitions.
    * Throws when the database fails, or when another application owns the queue.
    */
  def registerQueue(client: DBOSClient): Unit =
    client.registerQueue(
      QueueName,
      new QueueOptions().withPartitionConcurrency(1),
      QueueConflictResolution.ALWAYS_UPDATE,
      DurableWorkflow.ApplicationName
    )

  /** How an edge enqueues `turn`: under its workflow id, partitioned by its conversation. */
  def enqueueOptions(turn: TurnRef): DBOSClient.EnqueueOptions =
    new DBOSClient.EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, QueueName)
      .withWorkflowId(WorkflowId.value(turn.workflowId))
      .withQueuePartitionKey(ConversationId.value(turn.conversationId))
}
