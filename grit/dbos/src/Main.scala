package grit.dbos

import dev.dbos.transact.{DBOS, DBOSClient}
import dev.dbos.transact.config.DBOSConfig
import dev.dbos.transact.txstep.JdbcStepFactory
import grit.core.{Message, SourceId, WorkflowId}
import java.sql.DriverManager
import org.postgresql.ds.PGSimpleDataSource
import scala.io.Source
import scala.util.Using

/** The phase-0 proof run: an edge's whole path through Postgres. Ingests each argument
  * as a source message, starts its turn on the `turns` queue, and prints what the
  * stand-in [[ProofTurn]] saw, against the Postgres named by `GRIT_DATABASE_*` (see
  * [[DbConfig]]), defaulting to the local one.
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
    val ds = dataSource(config)

    // Each argument is a source message id; repeat one to watch the redelivery come back
    // as the same turn. With none, one fresh message.
    val sources =
      if (args.isEmpty) List("proof-" + System.currentTimeMillis()) else args.toList

    // The composition root: stores and the stand-in turn meet the DBOS adapter here.
    val conversations = new SqlConversationStore()
    val entries = new SqlEntryStore()
    Turns.registerQueue(dbos)
    DurableWorkflow.register(
      dbos,
      new JdbcStepFactory(dbos, ds),
      Turns.WorkflowName,
      ProofTurn.body(entries)
    )

    val failure: Option[String] =
      try {
        dbos.launch()
        // The edge's side: Postgres only, through the client.
        Using.resource(new DBOSClient(ds)) { client =>
          val inbox = new SqlInbox(ds, client, conversations, entries)
          val started = sources.map { source =>
            for {
              turn <- inbox.ingest(
                ProofTurn.ProofOrigin,
                SourceId(source),
                Message.User(s"proof message $source")
              )
              _ <- inbox.startTurn(turn)
            } yield turn
          }
          started.collectFirst { case Left(error) => error } match {
            case Some(error) => Some(s"inbox: $error")
            case None =>
              started.collect { case Right(turn) => turn }.distinct.foreach { turn =>
                val id = WorkflowId.value(turn.workflowId)
                val result = client.retrieveWorkflow[String, Exception](id).getResult()
                println(s"[main] $id -> $result")
              }
              None
          }
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
