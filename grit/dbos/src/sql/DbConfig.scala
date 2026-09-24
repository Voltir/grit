package grit.dbos.sql

/** Where grit's Postgres is and how to log in. `toString` never shows the
  * password.
  */
final case class DbConfig(jdbcUrl: String, user: String, password: String) {
  override def toString: String = s"DbConfig($jdbcUrl, $user, <redacted>)"
}

object DbConfig {

  val UrlVar = "GRIT_DATABASE_URL"
  val UserVar = "GRIT_DATABASE_USER"
  val PasswordVar = "GRIT_DATABASE_PASSWORD"

  /** `docker-compose.yml`'s database. */
  val Local: DbConfig =
    DbConfig("jdbc:postgresql://localhost:5432/grit", "grit", "grit")

  /** Why the environment does not name a usable database. Names the variable,
    * never its value: a URL can carry credentials.
    */
  enum Invalid {
    case Empty(variable: String)
    case NotPostgresJdbc(variable: String)

    def message: String = this match {
      case Empty(v) => s"$v is set but blank"
      case NotPostgresJdbc(v) => s"$v must be a jdbc:postgresql: URL"
    }
  }

  /** Reads `GRIT_DATABASE_URL`, `GRIT_DATABASE_USER` and
    * `GRIT_DATABASE_PASSWORD`. An unset variable takes its value from
    * [[Local]]; a set one is used as given. The URL and user must not be
    * blank, and the URL must be `jdbc:postgresql:`. The password may be empty.
    */
  def fromEnv(env: Map[String, String]): Either[Invalid, DbConfig] = {
    def nonBlank(variable: String, default: String): Either[Invalid, String] =
      env.get(variable) match {
        case None => Right(default)
        case Some(v) if v.trim.isEmpty => Left(Invalid.Empty(variable))
        case Some(v) => Right(v)
      }

    for {
      url <- nonBlank(UrlVar, Local.jdbcUrl)
      _ <- Either.cond(
        url.startsWith("jdbc:postgresql:"),
        (),
        Invalid.NotPostgresJdbc(UrlVar)
      )
      user <- nonBlank(UserVar, Local.user)
    } yield DbConfig(url, user, env.getOrElse(PasswordVar, Local.password))
  }
}
