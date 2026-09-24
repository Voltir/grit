package grit.dbos

import dev.dbos.transact.DBOS
import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.execution.RegisteredWorkflow
import grit.core.{Durable, WorkflowId}

/** The one class every grit workflow is registered under. DBOS records a workflow by its
  * name and class name, and refuses to resume an id under a different pair. This class's
  * name never changes, so a workflow's identity is its name alone, and its body can be any
  * function of its workflow id written against [[Durable]].
  */
final class DurableWorkflow private (
    dbos: DBOS,
    steps: JdbcStepFactory,
    body: WorkflowId => Durable^ ?=> String
) {

  /** The entry point DBOS calls reflectively, with no arguments. The workflow's only input
    * is its id, read from DBOS's context.
    */
  def run(): String = {
    val id = Option(DBOS.workflowId()).getOrElse {
      throw new IllegalStateException("DurableWorkflow.run called outside a DBOS workflow")
    }
    val workflowId = WorkflowId(id)
    body(workflowId)(using new DbosDurable(dbos, steps, workflowId))
  }
}

object DurableWorkflow {

  /** Registers `body` as the workflow `name`. Must run before `dbos.launch()`. */
  def register(
      dbos: DBOS,
      steps: JdbcStepFactory,
      name: String,
      body: WorkflowId => Durable^ ?=> String
  ): RegisteredWorkflow =
    dbos
      .integration()
      .registerWorkflow(
        name,
        classOf[DurableWorkflow].getName,
        null,
        new DurableWorkflow(dbos, steps, body),
        classOf[DurableWorkflow].getMethod("run"),
        null,
        null
      )
}
