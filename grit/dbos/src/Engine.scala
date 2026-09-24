package grit.dbos

import dev.dbos.transact.{DBOS, DBOSClient}
import dev.dbos.transact.config.DBOSConfig
import dev.dbos.transact.txstep.JdbcStepFactory
import grit.core.{
  ConversationStore,
  Db,
  Durable,
  EntryStore,
  Inbox,
  TurnRef,
  UsageLedger,
  WorkflowId
}
import java.sql.DriverManager
import org.postgresql.ds.PGSimpleDataSource
import scala.io.Source
import scala.util.Using

/** grit over one Postgres: the stores, the turn workflow, and an edge's [[Inbox]], all in
  * this process. Open it, [[launch]] it with the turn's body, and close it when done; its
  * threads keep the JVM alive until then.
  */
final class Engine private (dbos: DBOS, dataSource: PGSimpleDataSource)
    extends caps.SharedCapability,
      AutoCloseable {

  val conversations: ConversationStore = new SqlConversationStore()

  val entries: EntryStore = new SqlEntryStore()

  val ledger: UsageLedger = new SqlUsageLedger()

  /** Short read transactions, for code outside a step. */
  val db: Db = new SqlDb(dataSource)

  // An edge's side: it reaches the engine only through Postgres (ADR 0002).
  private val client = new DBOSClient(dataSource)

  val inbox: Inbox = new SqlInbox(dataSource, client, conversations, entries)

  /** Registers `turn` as the body of every turn and starts running queued turns. Once. */
  def launch(turn: WorkflowId => Durable^ ?=> String): Unit = {
    Turns.register(dbos, new JdbcStepFactory(dbos, dataSource), turn)
    dbos.launch()
  }

  /** Waits for `turn` to finish and returns its workflow's output. A turn that threw
    * rethrows here.
    */
  def awaitTurn(turn: TurnRef): String =
    client.retrieveWorkflow[String, Exception](WorkflowId.value(turn.workflowId)).getResult()

  def close(): Unit =
    try client.close()
    finally dbos.shutdown()
}

object Engine {

  /** Applies `schema.sql` to the database `config` names, and connects to it. The engine
    * recovers and dequeues only workflows of compatibility epoch `epoch` (ADR 0004).
    */
  def open(config: DbConfig, epoch: String): Engine^ = {
    schemaSetup(config)
    val dbos = new DBOS(
      DBOSConfig
        .defaults("grit")
        .withDatabaseUrl(config.jdbcUrl)
        .withDbUser(config.user)
        .withDbPassword(config.password)
        // Last in DBOS's precedence, so it beats both DBOS__APPVERSION and the constant
        // that enabling patching sets (DBOSExecutor's constructor).
        .withEnablePatching()
        .withAppVersion(epoch)
    )
    val ds = new PGSimpleDataSource()
    ds.setURL(config.jdbcUrl)
    ds.setUser(config.user)
    ds.setPassword(config.password)
    new Engine(dbos, ds)
  }

  /** Applies `grit/dbos/resources/schema.sql` idempotently. */
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
}
