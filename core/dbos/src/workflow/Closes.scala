package grit.dbos.workflow

import grit.core.durable.Durable
import grit.core.id.{CloseRef, ConversationId, WorkflowId}
import grit.dbos.sql.Opener

import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.{DBOS, EnqueueOptions}

/** How a close is known to DBOS: the workflow it runs as, on the turns' queue under its
  * conversation's partition, so DBOS never runs it beside a turn of the same conversation:
  * it waits behind one that is running, and runs after it.
  */
object Closes {

  private val WorkflowName = "close"

  /** Registers `body` as the close workflow. Must run before `dbos.launch()`. */
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

  /** How `attempt` is enqueued: under its workflow id, on the turns' queue, partitioned by its
    * conversation.
    */
  def enqueueOptions(attempt: CloseRef): EnqueueOptions =
    new EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, Turns.Queue)
      .withWorkflowId(WorkflowId.value(attempt.workflowId))
      .withQueuePartitionKey(ConversationId.value(attempt.period.conversationId))
}
