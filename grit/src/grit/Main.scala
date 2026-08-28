package grit

import dev.dbos.transact.{DBOS, StartWorkflowOptions}
import dev.dbos.transact.config.DBOSConfig
import dev.dbos.transact.execution.ThrowingSupplier
import dev.dbos.transact.workflow.StepOptions

/** Minimal "get DBOS running" slice. Registers one durable no-arg workflow and runs it
  * against a local Postgres.
  *
  * DBOS identifies workflows reflectively by class+method, so we register a Scala
  * Function0 and hand DBOS its `apply`.
  */
object Main {

  lazy val dbos: DBOS =
    new DBOS(
      DBOSConfig
        .defaults("grit")
        .withDatabaseUrl("jdbc:postgresql://localhost:5432/grit")
        .withDbUser("grit")
        .withDbPassword("grit")
    )

  /** One checkpointed step, exactly-once under replay. */
  private def work(): Unit = {
    println(s"[step] hello from DBOS workflow ${DBOS.workflowId()}")
  }

  /** Box a step result, mapping Scala Unit -> null for DBOS's Jackson. */
  private def box(value: => Any): AnyRef = {
    value match {
      case () => null
      case other => other.asInstanceOf[AnyRef]
    }
  }

  def main(args: Array[String]): Unit = {
    // Unique id per run so repeated invocations don't collide on idempotency.
    val wfId = args.headOption.getOrElse("hello-" + System.currentTimeMillis())

    val fn: Function0[AnyRef] = () => {
      val step: ThrowingSupplier[AnyRef, Exception] = () => box(work())
      dbos.runStep(step, new StepOptions("hello"))
      null
    }

    val method = classOf[Function0[?]].getMethod("apply")
    val registered = dbos
      .integration()
      .registerWorkflow("helloWorkflow", fn.getClass.getName, null, fn, method, null, null)

    dbos.launch()

    val handle = dbos
      .integration()
      .startRegisteredWorkflow(registered, Array.empty[AnyRef], new StartWorkflowOptions(wfId))

    val result = handle.getResult()
    println(s"[main] workflow completed: $result")

    dbos.shutdown()
  }
}
