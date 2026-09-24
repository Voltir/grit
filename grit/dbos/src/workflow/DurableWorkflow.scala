package grit.dbos.workflow

import grit.core.durable.Durable
import grit.core.id.WorkflowId

import dev.dbos.transact.DBOS
import dev.dbos.transact.execution.RegisteredWorkflow
import dev.dbos.transact.txstep.JdbcStepFactory

/** The one class every grit workflow is registered under. DBOS records a workflow by its
  * name and class name, and refuses to resume an id under a different pair. Every workflow
  * is recorded under the class name [[DurableWorkflow.ClassName]], so a workflow's identity
  * is its name alone, and its body can be any function of its workflow id written against
  * [[Durable]].
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

  /** The class name DBOS records for every grit workflow. It is persisted in every
    * workflow row, so it is a fixed string rather than this class's JVM name, which a
    * rename would change. DBOS uses it only as a lookup key, never to load a class.
    */
  val ClassName = "grit.workflow"

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
        ClassName,
        null,
        new DurableWorkflow(dbos, steps, body),
        classOf[DurableWorkflow].getMethod("run"),
        null,
        null
      )
}
