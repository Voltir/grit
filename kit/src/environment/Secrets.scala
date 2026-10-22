package grit.kit.environment

import java.nio.file.Path

import grit.dbos.sql.DbConfig
import grit.kit.deployment.{Deployment, Placer}
import grit.models.{JevConfig, OpenRouterConfig}

/** Why [[Secrets.of]] refused; each names the variable, never its value. */
enum SecretsRefusal {

  /** The database's variables name no usable database. */
  case Database(invalid: DbConfig.Invalid)

  /** A model's key is set but blank, or Jev's is unset for [[Placer.Jev]]. */
  case Key(invalid: OpenRouterConfig.Invalid)

  /** The file area's directory is named, but not as an absolute path: `why`. */
  case FilesDir(why: String)

  def message: String = this match {
    case Database(invalid) => invalid.message
    case Key(invalid) => invalid.message
    case FilesDir(why) => why
  }
}

/** What the process environment supplies the kit: the database (`GRIT_DATABASE_*`),
  * OpenRouter's key (`None`: the stub model, so no key, no spend), Jev's settings (`None`
  * unless the deployment places messages with Jev), and the directory the file area keeps its
  * bytes in (`GRIT_FILES_DIR`; `None`: one of the machine's temporary directory, by the
  * database, [[grit.kit.run.Launch.area]]).
  */
final case class Secrets private (
    database: DbConfig,
    openRouter: Option[String],
    jev: Option[JevConfig],
    files: Option[Path]
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
      jev <- deployment.placer match {
        case Placer.Jev => JevConfig.fromEnv(env).map(Some(_)).left.map(SecretsRefusal.Key(_))
        case Placer.Stub | Placer.Off(_) => Right(None)
      }
      files <- env.get(FilesDirVar) match {
        case None => Right(None)
        case Some(raw) =>
          scala.util
            .Try(Path.of(raw.trim))
            .toOption
            .filter(_.isAbsolute)
            .map(Some(_))
            .toRight(SecretsRefusal.FilesDir(s"$FilesDirVar must be an absolute path"))
      }
    } yield Secrets(database, openRouter, jev, files)

  /** The file area's directory, when set: `GRIT_FILES_DIR`. */
  val FilesDirVar: String = "GRIT_FILES_DIR"
}
