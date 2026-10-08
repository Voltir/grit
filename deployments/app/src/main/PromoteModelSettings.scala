package grit.app.main

import grit.core.model.{Catalog, CatalogJson, Profile}
import grit.core.spend.Budget
import grit.core.visibility.{Subject, Visibility}
import grit.dbos.engine.Engine
import grit.dbos.sql.DbConfig
import grit.kit.environment.DotEnv
import grit.models.Seed
import grit.turn.Turn

/** Prints the seed catalog with every model setting approved at runtime laid over it, in the seed's
  * form, for a person to put in `extensions/models/resources/catalog.json` and commit. The settings
  * are read from the Postgres `GRIT_DATABASE_*` names (a `.env` may set it), and nothing is
  * written.
  *
  * {{{./mill grit.app.runMain grit.app.main.PromoteModelSettings > extensions/models/resources/catalog.json}}}
  */
object PromoteModelSettings {

  /** `seed` with `approved` laid over it, oldest first, as the seed file holds a catalog;
    * ends with a newline.
    */
  def render(seed: Catalog, approved: Vector[Profile]): String =
    ujson.write(CatalogJson.write(seed.overlaid(approved)), indent = 2) + "\n"

  def main(args: Array[String]): Unit = {
    val _ = args
    val env = DotEnv
      .load(java.nio.file.Path.of(sys.env.getOrElse("GRIT_ENV_FILE", ".env")), sys.env)
      .fold(fail, identity)
    val config = DbConfig.fromEnv(env).left.map(_.message).fold(fail, identity)
    val seed = Seed.catalog.fold(fail, identity)
    // The engine's lock, like any engine: while grit runs on this database, this says so.
    val engine = Engine.open(
      config,
      Turn.Epoch,
      grit.host.LocalMachine.identity(),
      // It takes no messages: no cap, and any zone.
      Budget(java.time.ZoneOffset.UTC, None),
      // The reference deployment's: it declares none. A database that ran under compartments
      // is refused, naming one, rather than opened as if it had not.
      Visibility.Shipped
    ) match {
      case Right(open) => open
      case Left(refused) =>
        // clock-check: a one-shot command's composition root, read once for its refusal
        fail(refused.message(grit.core.clock.Clock.system().now()))
    }
    try
      engine.db.read(Subject.Public)(engine.modelSettings.all()) match {
        case Right(kept) => print(render(seed, kept.map(_.settings)))
        case Left(e) => fail(s"the approved model settings cannot be read: $e")
      }
    finally engine.close()
  }

  private def fail(message: String): Nothing = {
    System.err.println(s"[promote] $message")
    sys.exit(2)
  }
}
