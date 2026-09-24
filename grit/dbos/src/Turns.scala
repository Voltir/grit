package grit.dbos

import dev.dbos.transact.DBOS
import dev.dbos.transact.workflow.Queue

/** Where turns run: the workflow name every turn is registered under, and the queue that
  * runs one turn per conversation at a time, oldest first. DBOS counts running workflows
  * and dequeues per partition key, and the key is the conversation id.
  */
object Turns {

  val WorkflowName = "turn"

  val QueueName = "turns"

  /** Registers the `turns` queue. Must run before `dbos.launch()`. */
  def registerQueue(dbos: DBOS): Unit =
    dbos.registerQueue(new Queue(QueueName).withConcurrency(1).withPartitioningEnabled(true))
}
