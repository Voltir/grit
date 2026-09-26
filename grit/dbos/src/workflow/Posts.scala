package grit.dbos.workflow

import grit.core.durable.Durable
import grit.core.id.{PluginName, WorkflowId}
import grit.core.plugin.PostRef

import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.workflow.Queue
import dev.dbos.transact.{DBOS, DBOSClient}

/** How a plugin's posting run is known to DBOS: the workflow it runs as, and its own queue,
  * partitioned by plugin, so a plugin's runs go one at a time and never hold up a turn.
  */
object Posts {

  private val WorkflowName = "post"

  private val QueueName = "posts"

  /** Registers the `posts` queue and `body` as the posting workflow. Must run before
    * `dbos.launch()`.
    */
  def register(
      dbos: DBOS,
      steps: JdbcStepFactory,
      body: WorkflowId => Durable^ ?=> String
  ): Unit = {
    dbos.registerQueue(new Queue(QueueName).withConcurrency(1).withPartitioningEnabled(true))
    DurableWorkflow.register(dbos, steps, WorkflowName, body)
    ()
  }

  /** How `run` is enqueued: under its workflow id, partitioned by its plugin. */
  def enqueueOptions(run: PostRef): DBOSClient.EnqueueOptions =
    new DBOSClient.EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, QueueName)
      .withWorkflowId(WorkflowId.value(run.workflowId))
      .withQueuePartitionKey(PluginName.value(run.plugin))
}
