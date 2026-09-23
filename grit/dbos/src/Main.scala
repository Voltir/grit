package grit.dbos

import dev.dbos.transact.{DBOS, StartWorkflowOptions}
import dev.dbos.transact.config.DBOSConfig
import dev.dbos.transact.txstep.JdbcStepFactory
import java.sql.DriverManager
import org.postgresql.ds.PGSimpleDataSource
import scala.io.Source
import scala.util.Using

/** Minimal "get DBOS running" slice. Registers [[ProofWorkflow]] and runs it
  * once, under a given or fresh workflow id, against the Postgres named by
  * `GRIT_DATABASE_*` (see [[DbConfig]]), defaulting to the local one.
  */
object Main {

  /** Applies `grit/dbos/resources/schema.sql` idempotently before DBOS starts. */
  private def schemaSetup(config: DbConfig): Unit = {
    val sql = Option(getClass.getResourceAsStream("/schema.sql")) match {
      case Some(is) => Using.resource(is)(Source.fromInputStream(_).mkString)
      case None =>
        sys.error(
          "schema.sql not found on classpath — is grit/dbos/resources/ on the resource path?"
        )
    }
    Using.resource(
      DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    ) { conn =>
      Using.resource(conn.createStatement())(_.execute(sql))
    }
  }

  private def newDbos(config: DbConfig): DBOS =
    new DBOS(
      DBOSConfig
        .defaults("grit")
        .withDatabaseUrl(config.jdbcUrl)
        .withDbUser(config.user)
        .withDbPassword(config.password)
    )

  private def dataSource(config: DbConfig): PGSimpleDataSource = {
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    ds
  }

  def main(args: Array[String]): Unit = {
    val config = DbConfig.fromEnv(sys.env) match {
      case Right(c) => c
      case Left(invalid) =>
        System.err.println(s"[main] ${invalid.message}")
        sys.exit(2)
    }
    schemaSetup(config)
    val dbos = newDbos(config)

    // Unique id per run unless one is given; pass the same id twice to watch
    // the second run replay the first's recorded result.
    val wfId =
      args.headOption.getOrElse("hello-" + System.currentTimeMillis())

    // Construct the transaction seam — this is the composition root.
    val workflow =
      new ProofWorkflow(
        new Store(new JdbcStepFactory(dbos, dataSource(config))),
        new SqlConversationStore(),
        new SqlEntryStore()
      )

    val registered = dbos
      .integration()
      .registerWorkflow(
        ProofWorkflow.Name,
        classOf[ProofWorkflow].getName,
        null,
        workflow,
        classOf[ProofWorkflow].getMethod("run"),
        null,
        null
      )

    val failure: Option[String] =
      try {
        dbos.launch()

        if (dbos.getWorkflowStatus(wfId).isPresent) {
          println(s"[main] $wfId already recorded — replaying its result")
        }

        val handle = dbos
          .integration()
          .startRegisteredWorkflow(
            registered,
            Array.empty[AnyRef],
            new StartWorkflowOptions(wfId)
          )

        val result = handle.getResult()
        Option(result).collect { case s: String => s }.flatMap(Outcome.parse) match {
          case Some(Outcome.Inserted(id)) =>
            println(s"[main] inserted $id"); None
          case Some(Outcome.AlreadyPresent(id)) =>
            println(s"[main] $id already present"); None
          case Some(Outcome.Failed(detail)) =>
            Some(s"insert failed: $detail")
          case None =>
            Some(s"unrecognized workflow result: $result")
        }
      } finally {
        // DBOS's pool and executor threads are non-daemon: a throw that skips
        // shutdown leaves the JVM, and mill, waiting forever.
        dbos.shutdown()
      }

    failure.foreach { message =>
      System.err.println(s"[main] $message")
      sys.exit(1)
    }
  }
}
