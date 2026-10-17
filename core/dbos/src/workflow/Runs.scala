package grit.dbos.workflow

import grit.core.id.{ConversationId, TurnRef, WorkflowId}

import dev.dbos.transact.EnqueueOptions

/** How a job's run is known to DBOS (ADR 0029): a turn of its slot's conversation, run as its
  * own workflow, on the turns' queue under its conversation's partition, so it never runs
  * beside another turn or the close of that conversation.
  *
  * The engine registers and the inbox enqueues through this one object, so the two cannot name
  * the workflow differently.
  */
object Runs {

  private[dbos] val WorkflowName = "run"

  /** How `turn`, a run, is enqueued: under its workflow id, on the turns' queue, partitioned by
    * its conversation.
    */
  def enqueueOptions(turn: TurnRef): EnqueueOptions =
    new EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, Turns.Queue)
      .withWorkflowId(WorkflowId.value(turn.workflowId))
      .withQueuePartitionKey(ConversationId.value(turn.conversationId))
}
