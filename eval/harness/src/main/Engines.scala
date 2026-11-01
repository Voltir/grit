package grit.eval.harness.main

import java.time.ZoneOffset

import scala.util.control.NonFatal

import grit.core.clock.Clock
import grit.core.host.ProcessIdentity
import grit.core.visibility.Visibility
import grit.dbos.engine.Engine
import grit.dbos.sql.DbConfig

/** The engine a command opens for its turns, as an eval builder with no spend cap. */
private[main] object Engines {

  /** `f` of an engine opened on `config`'s database under `version`, closed after it; `Left` when
    * it cannot open (another engine holds the database's lock, named as of `clock`'s now), or
    * when `f` throws, saying what was not `done`.
    */
  private[main] def withEngine[A](config: DbConfig, clock: Clock^, version: String, done: String)(
      f: Engine^ => Either[String, A]
  ): Either[String, A] =
    try {
      Engine.open(config, version, Builder, Uncapped, Visibility.Shipped, clock) match {
        case Left(refused) => Left(s"the engine could not open: ${refused.message(clock.now())}")
        case Right(engine) =>
          try f(engine)
          finally engine.close()
      }
    } catch {
      case NonFatal(e) => Left(s"$done: ${e.getClass.getName}")
    }

  /** The process an engine the harness opens for itself says it is: no edge attaches to that
    * engine, so nothing need find this process by it.
    */
  private[main] val Builder: ProcessIdentity = ProcessIdentity("eval-synthetic", 0)

  /** What that engine takes new messages under: no cap, as none is ever sent it. */
  private[main] val Uncapped: grit.core.spend.Budget = grit.core.spend.Budget(ZoneOffset.UTC, None)
}
