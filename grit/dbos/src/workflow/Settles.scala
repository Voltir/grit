package grit.dbos.workflow

import grit.core.durable.Durable
import grit.core.id.{ConversationId, SettleRef, WorkflowId}

import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.{DBOS, DBOSClient}

/** How a settle is known to DBOS: the workflow it runs as, on the turns' queue under its
  * conversation's partition, as a close is ([[Closes]]), so it never runs beside a turn of
  * the same conversation.
  */
object Settles {

  private val WorkflowName = "settle"

  /** Registers `body` as the settle workflow. Must run before `dbos.launch()`, after
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

  /** How `question` is enqueued: under its workflow id, on the turns' queue, partitioned by
    * its conversation.
    */
  def enqueueOptions(question: SettleRef): DBOSClient.EnqueueOptions =
    new DBOSClient.EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, Turns.QueueName)
      .withWorkflowId(WorkflowId.value(question.workflowId))
      .withQueuePartitionKey(ConversationId.value(question.period.conversationId))
}
