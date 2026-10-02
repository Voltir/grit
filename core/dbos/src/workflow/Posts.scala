package grit.dbos.workflow

import grit.core.durable.Durable
import grit.core.id.{PluginName, WorkflowId}
import grit.core.plugin.PostRef

import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.workflow.QueueName
import dev.dbos.transact.workflow.{QueueConflictResolution, QueueOptions}
import dev.dbos.transact.{DBOS, DBOSClient, EnqueueOptions}

/** How a plugin's posting run is known to DBOS: the workflow it runs as, and its own queue,
  * partitioned by plugin, so a plugin's runs go one at a time and never hold up a turn.
  */
object Posts {

  private val WorkflowName = "post"

  private val Queue: QueueName = QueueName.of("posts")

  /** Registers `body` as the posting workflow. Must run before `dbos.launch()`. */
  def register(
      dbos: DBOS,
      steps: JdbcStepFactory,
      body: WorkflowId => Durable^ ?=> String,
      running: Running
  ): Unit = {
    DurableWorkflow.register(dbos, steps, WorkflowName, body, running)
    ()
  }

  /** Writes the `posts` queue to DBOS's database as application
    * [[DurableWorkflow.ApplicationName]]'s, replacing the configuration stored there: within
    * each partition (a plugin) one run at a time, oldest first, and no limit across
    * partitions. Throws when the database fails, or when another application owns the queue.
    */
  def registerQueue(client: DBOSClient): Unit =
    client.registerQueue(
      Queue.value,
      new QueueOptions().withPartitionConcurrency(1),
      QueueConflictResolution.ALWAYS_UPDATE,
      DurableWorkflow.ApplicationName
    )

  /** How `run` is enqueued: under its workflow id, partitioned by its plugin. */
  def enqueueOptions(run: PostRef): EnqueueOptions =
    new EnqueueOptions(WorkflowName, DurableWorkflow.ClassName, Queue)
      .withWorkflowId(WorkflowId.value(run.workflowId))
      .withQueuePartitionKey(PluginName.value(run.plugin))
}
