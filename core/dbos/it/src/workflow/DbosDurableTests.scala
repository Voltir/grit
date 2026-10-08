package grit.dbos.workflow

import java.sql.DriverManager
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Using

import grit.core.durable.{Durable, DurableContract, DurableRuntime, Settled}
import grit.core.id.WorkflowId
import grit.core.visibility.Visibility
import grit.dbos.sql.{DbConfig, Opener, TestPostgres}

import dev.dbos.transact.config.DBOSConfig
import dev.dbos.transact.exceptions.{DBOSNonExistentWorkflowException, DBOSUnexpectedStepException}
import dev.dbos.transact.execution.RegisteredWorkflow
import dev.dbos.transact.txstep.JdbcStepFactory
import dev.dbos.transact.workflow.WorkflowState
import dev.dbos.transact.{DBOS, StartWorkflowOptions}
import org.postgresql.ds.PGSimpleDataSource

/** [[DurableContract]] against [[DbosDurable]], on DBOS over a throwaway Postgres. */
object DbosDurableTests extends DurableContract {

  /* grit's schema applied first, as an engine's start applies it: a transaction step's
   * transaction reads what is recorded of rooms and groups as it opens. */
  private lazy val shared: DbosRuntime = {
    val c = TestPostgres.freshDatabase("durable")
    grit.dbos.engine.LiveEngine.open(c, "test").close()
    new DbosRuntime(c)
  }

  def runtime: DurableRuntime = shared

  override def utestAfterAll(): Unit = shared.close()
}

/** Thrown from a step body to stand for the process dying there. An `Error`, so DBOS's step
  * runner, which catches `Exception` (`DBOSExecutor.runStepInternal`), records nothing for
  * the step.
  */
final class ProcessDied extends Error("simulated process death")

/** DBOS over `config`'s database, with one registered workflow, `spec`, whose body is
  * whatever the current [[run]] of each id handed it.
  *
  * A crash leaves the database as a dead process would: the step unrecorded and the
  * workflow `PENDING`. The body catches [[ProcessDied]] and returns with the thread's
  * interrupt flag set, which makes DBOS skip recording the workflow's output
  * (`DBOSExecutor.executeWorkflow`: "interrupted before workflow.invoke completion").
  * `CrashFidelityTests` holds this to a JVM halted inside a step.
  *
  * The next run of a crashed id restarts: the old DBOS instance shuts down, then a new one
  * is launched, and its recovery resumes every `PENDING` workflow, as a restarted process's
  * would. Not beside the shutdown: recovery puts the workflow back on a queue (DBOS 1.1),
  * and the old instance, polling as the same executor, could claim it as it stops.
  */
final class DbosRuntime(config: DbConfig) extends DurableRuntime {

  private def newDbos(): DBOS = new DBOS(
    DBOSConfig
      .defaults("grit-durable-contract")
      .withDatabaseUrl(config.jdbcUrl)
      .withDbUser(config.user)
      .withDbPassword(config.password)
      .withEnablePatching()
      .withAppVersion("spec")
  )

  private val dataSource = {
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    ds
  }

  /** Each id's current body, and the latch its recovered run counts down when it settles. */
  private val bodies = new ConcurrentHashMap[String, WorkflowId -> Durable^ ?-> String]()
  private val settled = new ConcurrentHashMap[String, CountDownLatch]()
  private val crashed = ConcurrentHashMap.newKeySet[String]()

  /** A launched DBOS with the `spec` workflow registered on it. */
  private def launched(): (DBOS, RegisteredWorkflow) = {
    val dbos = newDbos()
    val registered = DurableWorkflow.register(
      dbos,
      new JdbcStepFactory(dbos, dataSource),
      new Opener(Visibility.Shipped),
      "spec",
      id => d ?=> dispatch(id),
      new Running
    )
    dbos.launch()
    (dbos, registered)
  }

  @caps.unsafe.untrackedCaptures
  private var current: (DBOS, RegisteredWorkflow) = launched()

  private def dbos: DBOS = current._1

  @caps.unsafe.untrackedCaptures
  private var issued = 0

  def freshId(): WorkflowId = {
    issued += 1
    WorkflowId(s"spec:$issued")
  }

  private def dispatch(id: WorkflowId)(using d: Durable^): String = {
    val key = WorkflowId.value(id)
    try
      Option(bodies.get(key)) match {
        case Some(body) => body(id)
        // Recovery resumes every PENDING workflow of this executor, not only the one a
        // run is waiting for. Any other is left as it was, like one this process never
        // reached before dying.
        case None => throw new ProcessDied
      }
    catch {
      case _: ProcessDied =>
        crashed.add(key)
        Thread.currentThread().interrupt()
        ""
    } finally Option(settled.get(key)).foreach(_.countDown())
  }

  def run(id: WorkflowId)(body: WorkflowId => Durable^ ?=> String): Settled = {
    val key = WorkflowId.value(id)
    // Cast, not unsafeAssumePure, which strips only the outer capture set and not the reach
    // capability of the context function inside. The body is called only while this run
    // waits for it to settle, and removed in the finally, so what it captures never
    // outlives the caller that handed it in.
    bodies.put(key, body.asInstanceOf[WorkflowId -> Durable^ ?-> String])
    crashed.remove(key)
    try running(key)
    finally {
      bodies.remove(key)
      settled.remove(key)
      ()
    }
  }

  private def running(key: String): Settled =
    if (status(key).contains(WorkflowState.PENDING)) {
      // The last run crashed: restart, and let recovery resume it.
      val latch = new CountDownLatch(1)
      settled.put(key, latch)
      dbos.shutdown()
      current = launched()
      if (!latch.await(30, TimeUnit.SECONDS)) sys.error(s"recovery never ran $key")
      outcome(key)
    } else {
      val handle = dbos
        .integration()
        .startRegisteredWorkflow(
          current._2,
          // DBOS only reads the arguments; this workflow takes none.
          caps.unsafe.unsafeAssumePure(Array.empty[Object]),
          new StartWorkflowOptions(key)
        )
      try {
        handle.getResult()
        outcome(key)
      } catch { case e: Exception => Settled.Threw(e) }
    }

  /** What `key` settled to once its body has returned: a crash, or its recorded outcome. */
  private def outcome(key: String): Settled =
    if (crashed.contains(key)) Settled.Crashed
    else {
      // The body has returned; DBOS records the outcome just after.
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10)
      while (status(key).forall(_ == WorkflowState.PENDING) && System.nanoTime() < deadline)
        Thread.sleep(5)
      try Settled.Returned(dbos.retrieveWorkflow[String, Exception](key).getResult())
      catch { case e: Exception => Settled.Threw(e) }
    }

  private def status(key: String): Option[WorkflowState] =
    dbos.getWorkflowStatus(key).toScala.map(_.status())

  def recordedSteps(id: WorkflowId): Vector[String] =
    dbos
      .listWorkflowSteps(WorkflowId.value(id))
      .asScala
      .toVector
      .sortBy(_.functionId())
      .map(_.functionName())

  def streamed(id: WorkflowId, key: String): Vector[String] =
    strings(
      """SELECT value FROM dbos.streams WHERE workflow_uuid = ? AND key = ? ORDER BY "offset"""",
      id,
      key
    )

  def crash(): Nothing = throw new ProcessDied

  def unexpectedStep(error: Throwable): Option[(String, String)] = error match {
    case e: DBOSUnexpectedStepException => Some((e.attemptedName(), e.recordedName()))
    case _ => None
  }

  def send(id: WorkflowId, topic: String, message: String, key: Option[String]): Unit =
    dbos.send(WorkflowId.value(id), message, topic, key.orNull)

  def noSuchWorkflow(error: Throwable): Option[WorkflowId] = error match {
    case e: DBOSNonExistentWorkflowException => Some(WorkflowId(e.workflowId()))
    case _ => None
  }

  def unreceived(id: WorkflowId, topic: String): Vector[String] =
    strings(
      """SELECT message FROM dbos.notifications
        |WHERE destination_uuid = ? AND topic = ? AND consumed = FALSE
        |ORDER BY created_at_epoch_ms""".stripMargin,
      id,
      topic
    )

  /** The first column of `sql`'s rows, each a JSON string as DBOS stores a `String`, for
    * the workflow `id` and a second parameter `second`.
    */
  private def strings(sql: String, id: WorkflowId, second: String): Vector[String] =
    Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
      conn =>
        Using.resource(conn.prepareStatement(sql)) { ps =>
          ps.setString(1, WorkflowId.value(id))
          ps.setString(2, second)
          Using.resource(ps.executeQuery()) { rs =>
            val out = Vector.newBuilder[String]
            while (rs.next()) out += ujson.read(rs.getString(1)).str
            out.result()
          }
        }
    }

  /** Shuts DBOS down. */
  def close(): Unit = dbos.shutdown()
}
