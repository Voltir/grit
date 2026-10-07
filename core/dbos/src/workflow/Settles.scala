package grit.dbos.workflow

import grit.core.durable.Durable
import grit.core.id.{ConversationId, SettleRef, WorkflowId}
import grit.dbos.sql.Opener

import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.{DBOS, EnqueueOptions}

/** How a settle is known to DBOS: the workflow it runs as, on the turns' queue under its
  * conversation's partition, as a close is ([[Closes]]), so it never runs beside a turn of
  * the same conversation.
  */
object Settles {

  private val WorkflowName = "settle"

  /** Registers `body` as the settle workflow. Must run before `dbos.launch()`. */
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

  /** How `question` is enqueued: under its workflow id, on the turns' queue, partitioned by
    * its conversation.
    */
  def enqueueOptions(question: SettleRef): EnqueueOptions =
    new EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, Turns.Queue)
      .withWorkflowId(WorkflowId.value(question.workflowId))
      .withQueuePartitionKey(ConversationId.value(question.period.conversationId))
}
