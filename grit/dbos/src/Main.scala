package grit.dbos

import dev.dbos.transact.{DBOS, StartWorkflowOptions}
import dev.dbos.transact.config.DBOSConfig
import dev.dbos.transact.txstep.JdbcStepFactory
import grit.core.{Entry, EntryId, StoreError}
import java.sql.DriverManager
import org.postgresql.ds.PGSimpleDataSource
import scala.io.Source
import scala.util.Using

/** Minimal "get DBOS running" slice. Registers one durable no-arg workflow
  * and runs it against a local Postgres.
  *
  * DBOS identifies workflows reflectively by class+method, so we register a
  * Scala Function0 and hand DBOS its `apply`.
  */
object Main {

  private val JdbcUrl = "jdbc:postgresql://localhost:5432/grit"
  private val DbUser = "grit"
  private val DbPassword = "grit"

  /** What the insert step reports back to the workflow.
    *
    * A step output crosses DBOS's Jackson boundary and is replayed from a row,
    * so it travels as a string; that encoding is confined to this object and
    * callers match on the enum.
    */
  private enum Outcome {
    case Inserted(id: String)
    case AlreadyPresent(id: String)
    case Failed(detail: String)
  }

  private object Outcome {
    def render(o: Outcome): String = o match {
      case Inserted(id) => s"inserted:$id"
      case AlreadyPresent(id) => s"already-present:$id"
      case Failed(detail) => s"failed:$detail"
    }

    def parse(s: String): Option[Outcome] = s.split(":", 2) match {
      case Array("inserted", id) => Some(Inserted(id))
      case Array("already-present", id) => Some(AlreadyPresent(id))
      case Array("failed", detail) => Some(Failed(detail))
      case _ => None
    }
  }

  /** Applies `grit/dbos/resources/schema.sql` idempotently before DBOS starts. */
  private def schemaSetup(): Unit = {
    val sql = Option(getClass.getResourceAsStream("/schema.sql")) match {
      case Some(is) => Using.resource(is)(Source.fromInputStream(_).mkString)
      case None =>
        sys.error(
          "schema.sql not found on classpath — is grit/dbos/resources/ on the resource path?"
        )
    }
    Using.resource(DriverManager.getConnection(JdbcUrl, DbUser, DbPassword)) { conn =>
      Using.resource(conn.createStatement())(_.execute(sql))
    }
  }

  lazy val dbos: DBOS =
    new DBOS(
      DBOSConfig
        .defaults("grit")
        .withDatabaseUrl(JdbcUrl)
        .withDbUser(DbUser)
        .withDbPassword(DbPassword)
    )

  private def dataSource(): PGSimpleDataSource = {
    val ds = new PGSimpleDataSource()
    ds.setURL(JdbcUrl)
    ds.setUser(DbUser)
    ds.setPassword(DbPassword)
    ds
  }

  def main(args: Array[String]): Unit = {
    schemaSetup()

    // Unique id per run so repeated invocations don't collide on idempotency.
    val wfId =
      args.headOption.getOrElse("hello-" + System.currentTimeMillis())

    // Construct the transaction seam — this is the composition root.
    val store = new Store(new JdbcStepFactory(dbos, dataSource()))
    val entries = new SqlEntryStore()

    // The step reports failure as a value rather than throwing: DBOS retries a
    // step that throws, and a duplicate insert is expected when this workflow
    // id replays.
    val fn: Function0[AnyRef] = () => {
      store.transact("insert-entry") { tx ?=>
        val entry = Entry(
          id = EntryId("proof-" + wfId.take(8)),
          parentId = None,
          seq = 0L,
          payload = ujson.Obj("note" -> ujson.Str("txStep proof")),
          createdAt = java.time.Instant.now()
        )
        val id = EntryId.value(entry.id)
        Outcome.render(entries.insert(entry) match {
          case Right(_) => Outcome.Inserted(id)
          case Left(StoreError.DuplicateId(_)) => Outcome.AlreadyPresent(id)
          case Left(StoreError.DatabaseError(cause)) => Outcome.Failed(cause)
        })
      }
    }

    val method = classOf[Function0[?]].getMethod("apply")
    val registered = dbos
      .integration()
      .registerWorkflow(
        "helloWorkflow",
        fn.getClass.getName,
        null,
        fn,
        method,
        null,
        null
      )

    dbos.launch()

    val handle = dbos
      .integration()
      .startRegisteredWorkflow(
        registered,
        Array.empty[AnyRef],
        new StartWorkflowOptions(wfId)
      )

    val result = handle.getResult()
    val outcome = Option(result).collect { case s: String => s }.flatMap(Outcome.parse)

    val failure: Option[String] = outcome match {
      case Some(Outcome.Inserted(id)) =>
        println(s"[main] inserted $id"); None
      case Some(Outcome.AlreadyPresent(id)) =>
        println(s"[main] $id already present — replay was idempotent"); None
      case Some(Outcome.Failed(detail)) =>
        Some(s"insert failed: $detail")
      case None =>
        Some(s"unrecognized workflow result: $result")
    }

    dbos.shutdown()

    failure.foreach { message =>
      System.err.println(s"[main] $message")
      sys.exit(1)
    }
  }
}
