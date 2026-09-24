package grit.dbos.sql

import java.sql.DriverManager

import scala.util.Using
import scala.util.control.NonFatal

import org.testcontainers.postgresql.PostgreSQLContainer
import org.testcontainers.utility.DockerImageName

/** One throwaway Postgres per test JVM, started by the first suite that asks, with a fresh
  * database per call so suites never see each other's rows. Nothing stops the container:
  * Testcontainers' Ryuk container removes it when the JVM exits.
  *
  * Needs Docker. Without it the first call fails and says so; there is no skip.
  */
object TestPostgres {

  private lazy val server: PostgreSQLContainer = {
    // Set by build.mill's LivePostgres from docker-compose.yml, so both run one image.
    val image = sys.env.getOrElse(
      "GRIT_POSTGRES_IMAGE",
      sys.error("GRIT_POSTGRES_IMAGE is unset: run tests through ./mill")
    )
    val container = new PostgreSQLContainer(DockerImageName.parse(image))
    // Test data is thrown away, so durability only slows the start and every write.
    container.withTmpFs(java.util.Map.of("/var/lib/postgresql", "rw"))
    // The one-String overload: the varargs one crashes Scala 3.9's capture checker
    // (docs/capture-checking.md).
    container.withCommand("postgres -c fsync=off -c synchronous_commit=off -c full_page_writes=off")
    try container.start()
    catch {
      case NonFatal(e) =>
        throw new IllegalStateException(
          s"Could not start $image in Docker. Live tests need a running Docker daemon.",
          e
        )
    }
    container
  }

  @caps.unsafe.untrackedCaptures
  private var created = 0

  /** A new, empty database on the shared server, named after `suite`. */
  def freshDatabase(suite: String): DbConfig = synchronized {
    created += 1
    val name = s"${suite.toLowerCase.replaceAll("[^a-z0-9]", "_")}_$created"
    Using.resource(
      DriverManager.getConnection(server.getJdbcUrl, server.getUsername, server.getPassword)
    ) { conn =>
      Using.resource(conn.createStatement())(_.execute(s"CREATE DATABASE $name"))
    }
    DbConfig(
      s"jdbc:postgresql://${server.getHost}:${server.getMappedPort(5432)}/$name",
      server.getUsername,
      server.getPassword
    )
  }
}
