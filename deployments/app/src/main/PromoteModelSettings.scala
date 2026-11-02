package grit.app.main

import grit.core.clock.Clock
import grit.core.model.{Catalog, CatalogJson}
import grit.dbos.sql.DbConfig
import grit.kit.deployment.Offered
import grit.kit.environment.{DotEnv, Exit}
import grit.kit.run.Kit
import grit.models.{KeptCatalog, Seed}

/** Prints the seed catalog with every model setting approved at runtime laid over it, in the seed's
  * form, for a person to put in `extensions/models/resources/catalog.json` and commit. The settings
  * are read from the Postgres `GRIT_DATABASE_*` names (a `.env` may set it: [[DotEnv.loadDefault]]), and nothing is
  * written.
  *
  * {{{./mill grit.app.runMain grit.app.main.PromoteModelSettings > extensions/models/resources/catalog.json}}}
  */
object PromoteModelSettings {

  /** `catalog` as the seed file holds one; ends with a newline. */
  def render(catalog: Catalog): String =
    ujson.write(CatalogJson.write(catalog), indent = 2) + "\n"

  def main(args: Array[String]): Unit = {
    val _ = args
    val clock: Clock^ = Clock.system() // clock-check: a one-shot command's composition root
    val exit = Exit.process("[promote]", 2)
    val env = exit(DotEnv.loadDefault(sys.env))
    val config = exit(DbConfig.fromEnv(env).left.map(_.message))
    val seed = exit(Seed.catalog)
    // The reference deployment, serving nothing: its engine opens under the deployment's
    // version, which a database running it made its latest (ADR 0004); any other would leave
    // the deployment's workflows undequeued until its next start.
    val deployment = exit(
      Deploying.deployment(env, Offered.Read, Vector.empty, Vector.empty, java.time.ZoneOffset.UTC)
    )
    // The engine's lock, like any engine: while grit runs on this database, this says so.
    // A database that ran under compartments the deployment does not declare is refused,
    // naming one, rather than opened as if it had not. It takes no messages, so the
    // deployment's budget caps nothing here.
    val engine = Kit.open(config, deployment, clock) match {
      case Right(open) => open
      case Left(refused) => exit.refuse(refused.message(clock.now()))
    }
    try print(render(exit(KeptCatalog.read(seed, engine.db, engine.modelSettings))))
    finally engine.close()
  }
}
