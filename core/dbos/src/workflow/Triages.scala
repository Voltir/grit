package grit.dbos.workflow

import grit.core.id.{ConversationId, TriageRef, WorkflowId}

import dev.dbos.transact.EnqueueOptions

/** How a triage is known to DBOS: the workflow it runs as, on the turns' queue under its
  * conversation's partition, as a close is ([[Closes]]), so it runs ahead of any close of its
  * conversation enqueued after it.
  */
object Triages {

  private[dbos] val WorkflowName = "triage"

  /** How `triage` is enqueued: under its workflow id, on the turns' queue, partitioned by its
    * conversation.
    */
  def enqueueOptions(triage: TriageRef): EnqueueOptions =
    new EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, Turns.Queue)
      .withWorkflowId(WorkflowId.value(triage.workflowId))
      .withQueuePartitionKey(ConversationId.value(triage.period.conversationId))
}
