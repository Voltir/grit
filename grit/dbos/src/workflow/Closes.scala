package grit.dbos.workflow

import grit.core.durable.Durable
import grit.core.id.{CloseRef, ConversationId, WorkflowId}

import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.{DBOS, DBOSClient}

/** How a close is known to DBOS: the workflow it runs as, on the turns' queue under its
  * conversation's partition, so DBOS never runs it beside a turn of the same conversation:
  * it waits behind one that is running, and runs after it.
  */
object Closes {

  private val WorkflowName = "close"

  /** Registers `body` as the close workflow. Must run before `dbos.launch()`, after
    * [[Turns.register]], which registers the queue.
    */
  def register(
      dbos: DBOS,
      steps: JdbcStepFactory,
      body: WorkflowId => Durable^ ?=> String
  ): Unit = {
    DurableWorkflow.register(dbos, steps, WorkflowName, body)
    ()
  }

  /** How `attempt` is enqueued: under its workflow id, on the turns' queue, partitioned by its
    * conversation.
    */
  def enqueueOptions(attempt: CloseRef): DBOSClient.EnqueueOptions =
    new DBOSClient.EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, Turns.QueueName)
      .withWorkflowId(WorkflowId.value(attempt.workflowId))
      .withQueuePartitionKey(ConversationId.value(attempt.period.conversationId))
}
