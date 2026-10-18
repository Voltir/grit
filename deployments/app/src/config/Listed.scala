package grit.app.config

/** A setting written as a comma-separated list. */
object Listed {

  /** The items `variable` lists in `env`, each trimmed, blank ones dropped; none when unset. */
  def csv(env: Map[String, String], variable: String): Vector[String] =
    env.get(variable).fold(Vector.empty)(_.split(',').toVector.map(_.trim).filter(_.nonEmpty))
}
