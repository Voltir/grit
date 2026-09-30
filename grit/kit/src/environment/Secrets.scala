package grit.kit.environment

import grit.dbos.sql.DbConfig
import grit.kit.deployment.{Deployment, Topics}
import grit.models.{JevConfig, OpenRouterConfig}

/** Why [[Secrets.of]] refused; each names the variable, never its value. */
enum SecretsRefusal {

  /** The database's variables name no usable database. */
  case Database(invalid: DbConfig.Invalid)

  /** A model's key is set but blank, or Jev's is unset for [[Topics.Jev]]. */
  case Key(invalid: OpenRouterConfig.Invalid)

  def message: String = this match {
    case Database(invalid) => invalid.message
    case Key(invalid) => invalid.message
  }
}

/** What the process environment supplies the kit: the database (`GRIT_DATABASE_*`),
  * OpenRouter's key (`None`: the stub model, so no key, no spend) and Jev's settings (`None`
  * unless the deployment places topics with Jev).
  */
final case class Secrets private (
    database: DbConfig,
    openRouter: Option[String],
    jev: Option[JevConfig]
)

object Secrets {

  /** `env`'s secrets for `deployment`: the first variable missing or malformed that it needs
    * refuses it.
    */
  def of(env: Map[String, String], deployment: Deployment): Either[SecretsRefusal, Secrets] =
    for {
      database <- DbConfig.fromEnv(env).left.map(SecretsRefusal.Database(_))
      openRouter <-
        if (!env.contains(OpenRouterConfig.KeyVar)) Right(None)
        else OpenRouterConfig.key(env).map(Some(_)).left.map(SecretsRefusal.Key(_))
      jev <- deployment.topics match {
        case Topics.Jev => JevConfig.fromEnv(env).map(Some(_)).left.map(SecretsRefusal.Key(_))
        case Topics.Stub | Topics.Off(_) => Right(None)
      }
    } yield Secrets(database, openRouter, jev)
}
