package grit.app.config

import grit.core.identity.Domain

/** The email domains the reference deployment claims as its own (ADR 0032), as a person sets
  * them: `GRIT_CLAIMED_DOMAINS`.
  */
object Claimed {

  val DomainsVar = "GRIT_CLAIMED_DOMAINS"

  /** The domains `GRIT_CLAIMED_DOMAINS` lists, comma-separated, each as [[Domain.of]] reads it;
    * none when it is unset or blank. Why not, naming the variable and the first domain refused.
    */
  def fromEnv(env: Map[String, String]): Either[String, Set[Domain]] =
    env
      .get(DomainsVar)
      .fold(Vector.empty[String])(_.split(",").toVector.map(_.trim).filter(_.nonEmpty))
      .foldLeft[Either[String, Set[Domain]]](Right(Set.empty)) { (so, raw) =>
        so.flatMap(domains => Domain.of(raw).map(domains + _).left.map(why => s"$DomainsVar: $why"))
      }
}
