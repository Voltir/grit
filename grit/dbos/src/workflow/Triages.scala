package grit.dbos.workflow

import grit.core.durable.Durable
import grit.core.id.{ConversationId, TriageRef, WorkflowId}

import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.{DBOS, DBOSClient}

/** How a triage is known to DBOS: the workflow it runs as, on the turns' queue under its
  * conversation's partition, as a close is ([[Closes]]), so it runs ahead of any close of its
  * conversation enqueued after it.
  */
object Triages {

  private val WorkflowName = "triage"

  /** Registers `body` as the triage workflow. Must run before `dbos.launch()`, after
    * [[Turns.register]], which registers the queue.
    */
  def register(
      dbos: DBOS,
      steps: JdbcStepFactory,
      body: WorkflowId => Durable^ ?=> String,
      running: Running
  ): Unit = {
    DurableWorkflow.register(dbos, steps, WorkflowName, body, running)
    ()
  }

  /** How `triage` is enqueued: under its workflow id, on the turns' queue, partitioned by its
    * conversation.
    */
  def enqueueOptions(triage: TriageRef): DBOSClient.EnqueueOptions =
    new DBOSClient.EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, Turns.QueueName)
      .withWorkflowId(WorkflowId.value(triage.workflowId))
      .withQueuePartitionKey(ConversationId.value(triage.period.conversationId))
}
