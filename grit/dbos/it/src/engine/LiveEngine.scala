package grit.dbos.engine

import java.time.Instant

import grit.core.host.ProcessIdentity
import grit.dbos.sql.DbConfig

/** An engine on a live test database, for suites that need one. */
object LiveEngine {

  /** The process every test engine and edge says it is: fixed, so a test can name it. */
  val Identity: ProcessIdentity = ProcessIdentity("test-machine", 4242)

  /** The engine of `config`'s database under compatibility epoch `epoch`; throws, naming the
    * holder, when another engine holds its lock: a test that opens two at once has a bug.
    */
  def open(config: DbConfig, epoch: String): Engine^ =
    Engine.open(config, epoch, Identity) match {
      case Right(engine) => engine
      case Left(refused) =>
        sys.error(s"the engine could not open: ${refused.message(Instant.now())}")
    }
}
