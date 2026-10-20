package grit.dbos.sql

/** Where grit's Postgres is and how to log in. `toString` never shows the
  * password.
  */
final case class DbConfig(jdbcUrl: String, user: String, password: String) {
  override def toString: String = s"DbConfig(${endpoint}, $user, <redacted>)"

  /** The hosts and database `jdbcUrl` names, never its parameters: those can carry
    * credentials.
    */
  def endpoint: DbConfig.Endpoint = DbConfig.Endpoint.of(jdbcUrl)
}

object DbConfig {

  val UrlVar = "GRIT_DATABASE_URL"
  val UserVar = "GRIT_DATABASE_USER"
  val PasswordVar = "GRIT_DATABASE_PASSWORD"

  /** Where a `jdbc:postgresql:` URL points. `hosts` is as the URL writes them, each with its
    * port, 5432 when it names none; `localhost:5432` when it names no host. `database` is
    * `None` when the URL names none. `toString` is `hosts/database`.
    */
  final case class Endpoint(hosts: String, database: Option[String]) {
    override def toString: String = s"$hosts/${database.getOrElse("")}"
  }

  object Endpoint {

    /** The endpoint `url` names, read as pgjdbc reads a `jdbc:postgresql:` URL. */
    def of(url: String): Endpoint = {
      val rest = url.stripPrefix("jdbc:postgresql:")
      val (hosts, path) =
        if (rest.startsWith("//")) {
          val (authority, path) = rest.drop(2).span(_ != '/')
          val named = authority.split(',').toVector.filter(_.nonEmpty).map { host =>
            // No port when no `:` follows an IPv6 literal's `]`.
            if (host.lastIndexOf(':') <= host.lastIndexOf(']')) s"$host:5432" else host
          }
          (if (named.isEmpty) "localhost:5432" else named.mkString(","), path.drop(1))
        } else ("localhost:5432", rest.stripPrefix("/"))
      Endpoint(hosts, Some(path.takeWhile(_ != '?')).filter(_.nonEmpty))
    }
  }

  /** Why the environment does not name a usable database. Names the variable,
    * never its value: a URL can carry credentials.
    */
  enum Invalid {

    /** `GRIT_DATABASE_URL` is unset: grit never picks a database for itself. */
    case Unset
    case Empty(variable: String)
    case NotPostgresJdbc(variable: String)

    def message: String = this match {
      case Unset =>
        s"$UrlVar is unset: set it to grit's database, as jdbc:postgresql://host:port/db"
      case Empty(v) => s"$v is set but blank"
      case NotPostgresJdbc(v) => s"$v must be a jdbc:postgresql: URL"
    }
  }

  /** Reads `GRIT_DATABASE_URL`, `GRIT_DATABASE_USER` and `GRIT_DATABASE_PASSWORD`. The URL
    * must be set, not blank, and `jdbc:postgresql:`. The user and password, when unset, are
    * `grit` and `grit`, `docker-compose.yml`'s login; the user must not be blank, the password
    * may be empty.
    */
  def fromEnv(env: Map[String, String]): Either[Invalid, DbConfig] =
    for {
      url <- env.get(UrlVar) match {
        case None => Left(Invalid.Unset)
        case Some(v) if v.trim.isEmpty => Left(Invalid.Empty(UrlVar))
        case Some(v) => Right(v)
      }
      _ <- Either.cond(
        url.startsWith("jdbc:postgresql:"),
        (),
        Invalid.NotPostgresJdbc(UrlVar)
      )
      user <- env.get(UserVar) match {
        case None => Right("grit")
        case Some(v) if v.trim.isEmpty => Left(Invalid.Empty(UserVar))
        case Some(v) => Right(v)
      }
    } yield DbConfig(url, user, env.getOrElse(PasswordVar, "grit"))
}
