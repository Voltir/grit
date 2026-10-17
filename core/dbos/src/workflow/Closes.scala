package grit.dbos.workflow

import grit.core.id.{CloseRef, ConversationId, WorkflowId}

import dev.dbos.transact.EnqueueOptions

/** How a close is known to DBOS: the workflow it runs as, on the turns' queue under its
  * conversation's partition, so DBOS never runs it beside a turn of the same conversation:
  * it waits behind one that is running, and runs after it.
  */
object Closes {

  private[dbos] val WorkflowName = "close"

  /** How `attempt` is enqueued: under its workflow id, on the turns' queue, partitioned by its
    * conversation.
    */
  def enqueueOptions(attempt: CloseRef): EnqueueOptions =
    new EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, Turns.Queue)
      .withWorkflowId(WorkflowId.value(attempt.workflowId))
      .withQueuePartitionKey(ConversationId.value(attempt.period.conversationId))
}
