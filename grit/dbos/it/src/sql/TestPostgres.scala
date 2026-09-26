package grit.dbos.sql

import java.nio.file.Paths
import java.sql.DriverManager

import scala.util.Using
import scala.util.control.NonFatal

import org.testcontainers.images.builder.ImageFromDockerfile
import org.testcontainers.postgresql.PostgreSQLContainer
import org.testcontainers.utility.DockerImageName

/** One throwaway Postgres per test run, with a fresh database per call so suites never see
  * each other's rows. The server is the one `GRIT_TEST_POSTGRES` names (`host:port`, user and
  * password `test`) when it is set -- `scripts/it` starts one for every integration JVM --
  * and otherwise one Testcontainers container per JVM, started by the first suite that asks
  * and removed by Testcontainers' Ryuk container when the JVM exits.
  *
  * Needs Docker. Without it the first call fails and says so; there is no skip.
  */
object TestPostgres {

  /** Where to reach the server: host, port, user, password. */
  private final case class Server(host: String, port: Int, user: String, password: String) {
    def url(database: String): String = s"jdbc:postgresql://$host:$port/$database"
  }

  private lazy val server: Server =
    sys.env.get("GRIT_TEST_POSTGRES").filter(_.nonEmpty) match {
      case Some(address) =>
        address.split(':') match {
          case Array(host, port) if port.toIntOption.isDefined =>
            // scripts/it starts the container with these credentials.
            Server(host, port.toInt, "test", "test")
          case _ => sys.error(s"GRIT_TEST_POSTGRES is not host:port: $address")
        }
      case None =>
        val c = container
        Server(c.getHost, c.getMappedPort(5432), c.getUsername, c.getPassword)
    }

  private def container: PostgreSQLContainer = {
    // Set by build.mill's LivePostgres: compose's tag, and the Dockerfile compose builds it
    // from, so both run one image.
    def env(name: String): String =
      sys.env.getOrElse(name, sys.error(s"$name is unset: run tests through ./mill"))
    val image = env("GRIT_POSTGRES_IMAGE")
    // Built by every JVM that starts its own container, under compose's tag and kept
    // afterwards. Docker's layer cache makes a rebuild a no-op until the Dockerfile changes,
    // and building (rather than using the tag if present) means a stale local tag can never
    // stand in for the file. scripts/it builds it the same way.
    val built =
      try {
        new ImageFromDockerfile(image, false)
          .withDockerfile(Paths.get(env("GRIT_POSTGRES_DOCKERFILE")))
          .get()
      } catch {
        case NonFatal(e) =>
          throw new IllegalStateException(
            s"Could not build $image in Docker. Live tests need a running Docker daemon.",
            e
          )
      }
    val container =
      new PostgreSQLContainer(DockerImageName.parse(built).asCompatibleSubstituteFor("postgres"))
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

  /** A new, empty database on the run's server, named after `suite`, and unique among the
    * JVMs sharing that server.
    */
  def freshDatabase(suite: String): DbConfig = synchronized {
    created += 1
    val pid = ProcessHandle.current().pid()
    val name = s"${suite.toLowerCase.replaceAll("[^a-z0-9]", "_")}_${pid}_$created"
    Using.resource(
      DriverManager.getConnection(server.url("postgres"), server.user, server.password)
    ) { conn =>
      Using.resource(conn.createStatement())(_.execute(s"CREATE DATABASE $name"))
    }
    DbConfig(server.url(name), server.user, server.password)
  }
}
