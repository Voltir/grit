package grit.dbos.engine

import java.time.Instant

import grit.dbos.sql.DbConfig

/** An engine on a live test database, for suites that need one. */
object LiveEngine {

  /** The engine of `config`'s database under compatibility epoch `epoch`; throws, naming the
    * holder, when another engine holds its lock: a test that opens two at once has a bug.
    */
  def open(config: DbConfig, epoch: String): Engine^ =
    Engine.open(config, epoch) match {
      case Right(engine) => engine
      case Left(refused) =>
        sys.error(s"the engine could not open: ${refused.message(Instant.now())}")
    }
}
