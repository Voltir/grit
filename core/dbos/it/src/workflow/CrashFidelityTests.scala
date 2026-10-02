package grit.dbos.workflow

import java.sql.DriverManager
import java.util.concurrent.TimeUnit

import scala.jdk.CollectionConverters.*
import scala.util.Using

import grit.core.durable.{Durable, Settled}
import grit.core.id.WorkflowId
import grit.dbos.sql.{DbConfig, TestPostgres}

import utest.*

/** Whether [[DbosRuntime]]'s in-process crash leaves the database as a process that really
  * died inside a step does. The real death is a child JVM that halts inside the step
  * ([[CrashingChild]]).
  *
  * The in-process crash relies on two DBOS internals, so this suite is the check to run on
  * a DBOS upgrade: `DBOSExecutor.runStepInternal` catches only `Exception`, so an `Error`
  * thrown from a step leaves it unrecorded; and `DBOSExecutor.executeWorkflow` skips
  * recording the output of a workflow whose thread was interrupted, so it stays `PENDING`.
  */
object CrashFidelityTests extends TestSuite {

  /** What a crashed workflow left in DBOS's tables, without ids or timestamps: its status
    * row, its payloads (DBOS 1.2 keeps them apart from the status), its steps and its stream.
    */
  final case class Remains(
      status: Vector[String],
      payloads: Vector[String],
      steps: Vector[String],
      streams: Vector[String]
  )

  def left(config: DbConfig, id: WorkflowId): Remains =
    Using.resource(DriverManager.getConnection(config.jdbcUrl, config.user, config.password)) {
      conn =>
        def rows(sql: String, columns: Int): Vector[String] =
          Using.resource(conn.prepareStatement(sql)) { ps =>
            ps.setString(1, WorkflowId.value(id))
            Using.resource(ps.executeQuery()) { rs =>
              val out = Vector.newBuilder[String]
              while (rs.next())
                out += (1 to columns).map(i => String.valueOf(rs.getObject(i))).mkString("|")
              out.result()
            }
          }
        Remains(
          rows(
            """SELECT status, recovery_attempts, executor_id, application_version, name,
              |class_name FROM dbos.workflow_status WHERE workflow_uuid = ?""".stripMargin,
            6
          ),
          rows(
            """SELECT kind, a, b FROM (
              |  SELECT 'input' AS kind, workflow_uuid, inputs AS a, NULL AS b FROM dbos.workflow_input
              |  UNION ALL
              |  SELECT 'output', workflow_uuid, output, error FROM dbos.workflow_output
              |) p WHERE workflow_uuid = ? ORDER BY kind""".stripMargin,
            3
          ),
          rows(
            """SELECT function_id, function_name, output, error FROM dbos.operation_outputs
              |WHERE workflow_uuid = ? ORDER BY function_id""".stripMargin,
            4
          ),
          rows(
            """SELECT "offset", function_id, value FROM dbos.streams WHERE workflow_uuid = ?
              |ORDER BY "offset"""".stripMargin,
            3
          )
        )
    }

  final class Counts {
    @caps.unsafe.untrackedCaptures
    var a = 0
    @caps.unsafe.untrackedCaptures
    var b = 0
  }

  /** Step `a`, then step `b`, which writes a piece and then does whatever `inB` does. */
  def twoSteps(counts: Counts, inB: () => Unit)(using d: Durable^): String = {
    val out = d.stream("k")
    val a = d.step("a") { () => counts.a += 1; "a" }
    val b = d.step("b") { () =>
      counts.b += 1
      out.write("in b")
      inB()
      "b"
    }
    a + b
  }

  /** Runs [[CrashingChild]] on `config`'s database for `id`, and returns its exit code. */
  private def halt(config: DbConfig, id: WorkflowId): Int = {
    val command = Vector(sys.env("GRIT_TEST_JAVA")) ++
      sys.env("GRIT_TEST_FORK_ARGS").split(" ").filter(_.nonEmpty) ++
      Vector("-cp", sys.env("GRIT_TEST_CLASSPATH"), "grit.dbos.workflow.CrashingChild") :+
      WorkflowId.value(id)
    val child = new ProcessBuilder(command.asJava)
      .redirectErrorStream(true)
      .redirectOutput(ProcessBuilder.Redirect.DISCARD)
    val env = child.environment()
    env.put("GRIT_DATABASE_URL", config.jdbcUrl)
    env.put("GRIT_DATABASE_USER", config.user)
    env.put("GRIT_DATABASE_PASSWORD", config.password)
    val process = child.start()
    try {
      if (!process.waitFor(60, TimeUnit.SECONDS)) sys.error("the crashing child never exited")
      process.exitValue()
    } finally process.destroyForcibly()
  }

  val tests = Tests {
    test("an in-process crash leaves the tables as a halted process does, and both recover") {
      val config = TestPostgres.freshDatabase("crash_fidelity")
      val rt = new DbosRuntime(config)
      try {
        val halted = WorkflowId("halted")
        halt(config, halted) ==> CrashingChild.Halted
        // After the child: a launch recovers every PENDING workflow of its executor, so a
        // child launched later would resume this crash too.
        val inProcess = WorkflowId("in-process")
        val counts = new Counts
        rt.run(inProcess)(_ => twoSteps(counts, () => rt.crash())) ==> Settled.Crashed
        left(config, inProcess) ==> left(config, halted)

        // Both resume on the next launch; the halted one ran step a in the child.
        val resumed = new Counts
        rt.run(halted)(_ => twoSteps(resumed, () => ())) ==> Settled.Returned("ab")
        (resumed.a, resumed.b) ==> (0, 1)
        rt.run(inProcess)(_ => twoSteps(counts, () => ())) ==> Settled.Returned("ab")
        (counts.a, counts.b) ==> (1, 2)
        rt.streamed(halted, "k") ==> Vector("in b", "in b")
      } finally rt.close()
    }
  }
}

/** A process that dies inside a step: runs the workflow named by its argument on the
  * database named by `GRIT_DATABASE_*`, and halts the JVM with [[Halted]] inside its step
  * `b`.
  */
object CrashingChild {

  val Halted = 37

  def main(args: Array[String]): Unit = {
    val config = DbConfig.fromEnv(sys.env).fold(e => sys.error(e.message), identity)
    val id = args.headOption.fold(sys.error("usage: CrashingChild <workflow id>"))(WorkflowId(_))
    val rt = new DbosRuntime(config)
    rt.run(id)(_ =>
      CrashFidelityTests.twoSteps(
        new CrashFidelityTests.Counts,
        () => Runtime.getRuntime.halt(Halted)
      )
    )
    // Reaching here means the halt never fired.
    rt.close()
    sys.exit(1)
  }
}
