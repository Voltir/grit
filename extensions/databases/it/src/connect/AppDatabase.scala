package grit.databases.connect

import java.sql.DriverManager

import scala.concurrent.duration.*

import grit.core.place.Service
import grit.core.secret.Variable
import grit.databases.declare.{Database, DatabaseRole, Limits, Login}
import grit.dbos.sql.{DbConfig, TestPostgres}

/** A database shaped like an application's, made fresh on the run's throwaway Postgres, with its
  * owner's admin connection for making roles and grants; and the declarations a test opens it
  * by. Roles are the server's, not the database's, so each name a test asks for is made unique
  * to this database.
  *
  * Its schema `app`: `leads` (`id`, `external_id`, a comment on each), `applicants` (`id`,
  * `lead_id` referring to `leads.id`, `name`, `ssn`), `scratch` (`n`), a view `lead_ids`, and
  * `hidden` (`secret`); five leads, one with no `external_id`, and two applicants.
  */
final class AppDatabase private (config: DbConfig, val name: String) {

  /** The JDBC URL of this database. */
  def url: String = config.jdbcUrl

  /** `statements` run as the server's superuser on this database, in order, each committed. */
  def admin(statements: String*): Unit = {
    val c = DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    try {
      val st = c.createStatement()
      try statements.foreach(st.execute)
      finally st.close()
    } finally c.close()
  }

  /** The first column of `query`'s first row, as text, run as the superuser. */
  def one(query: String): String = {
    val c = DriverManager.getConnection(config.jdbcUrl, config.user, config.password)
    try {
      val rs = c.createStatement().executeQuery(query)
      if (rs.next()) rs.getString(1) else ""
    } finally c.close()
  }

  /** `role` made unique to this database. */
  def role(role: String): String = s"${role}_$name"

  /** A login role `role(base)`, `NOINHERIT`, with password [[AppDatabase.Password]], allowed to
    * connect and to use `app`, then granted `grants`, each a `GRANT … TO` clause's start
    * (`"SELECT ON app.leads"`), and made with `attributes` besides (`"SUPERUSER"`).
    */
  def login(base: String, grants: Seq[String], attributes: String = ""): String = {
    val r = role(base)
    admin(
      (Seq(
        s"""CREATE ROLE "$r" LOGIN NOINHERIT $attributes PASSWORD '${AppDatabase.Password}'""",
        s"""GRANT CONNECT ON DATABASE "$name" TO "$r"""",
        s"""GRANT USAGE ON SCHEMA app TO "$r""""
      ) ++ grants.map(g => s"""GRANT $g TO "$r""""))*
    )
    r
  }

  /** The reader's declaration for logging in as `user` and being `role`, under `limits`. */
  def database(role: String, limits: Limits = Limits.Default): Database =
    (for {
      service <- Service.of("leads")
      r <- DatabaseRole.of(role)
    } yield Database(
      service,
      AppDatabase.Url,
      Login.Password(AppDatabase.User, AppDatabase.Pass),
      r,
      "Leads.",
      limits
    )) match {
      case Right(d) => d
      case Left(why) => throw new IllegalStateException(why)
    }

  /** The environment logging in as `user` with [[AppDatabase.Password]], at `url`. */
  def env(user: String, url: String = url): Map[String, String] = Map(
    Variable.value(AppDatabase.Url) -> url,
    Variable.value(AppDatabase.User) -> user,
    Variable.value(AppDatabase.Pass) -> AppDatabase.Password
  )

  /** A reader opened as `user`, declared as `role` (`user` by default), or the test fails. */
  def reader(
      user: String,
      role: Option[String] = None,
      limits: Limits = Limits.Default
  ): DatabaseReader =
    DatabaseReader.open(database(role.getOrElse(user), limits), env(user)) match {
      case Right(r) => r
      case Left(why) => throw new IllegalStateException(why.message)
    }
}

object AppDatabase {

  val Url: Variable = Variable("APP_DB_URL")
  val User: Variable = Variable("APP_DB_USER")
  val Pass: Variable = Variable("APP_DB_PASSWORD")

  /** Every test role's password. */
  val Password = "reader-password"

  /** A fresh database for `suite`, its schema made and filled. */
  def apply(suite: String): AppDatabase = {
    val config = TestPostgres.freshDatabase(suite)
    val name = config.jdbcUrl.substring(config.jdbcUrl.lastIndexOf('/') + 1)
    val db = new AppDatabase(config, name)
    db.admin(
      "CREATE SCHEMA app",
      "CREATE TABLE app.leads (id int PRIMARY KEY, external_id text)",
      "COMMENT ON TABLE app.leads IS 'One row per lead the application took in.'",
      "COMMENT ON COLUMN app.leads.id IS 'Ours, in the offers URL.'",
      "COMMENT ON COLUMN app.leads.external_id IS 'The one sent to the supplier.'",
      "CREATE TABLE app.applicants (id int PRIMARY KEY, lead_id int REFERENCES app.leads (id), name text, ssn text)",
      "CREATE TABLE app.scratch (n int)",
      "CREATE TABLE app.hidden (secret text)",
      "CREATE VIEW app.lead_ids AS SELECT id FROM app.leads",
      "INSERT INTO app.leads VALUES (1, 'x-1'), (2, 'x-2'), (3, NULL), (4, 'x-4'), (5, 'x-5')",
      "INSERT INTO app.applicants VALUES (1, 1, 'Ada', '000-00-0001'), (2, 2, 'Bo', '000-00-0002')",
      "INSERT INTO app.hidden VALUES ('kept')"
    )
    db
  }

  /** A short limit: `rows` rows, `bytes` bytes, `timeout`. */
  def limits(
      rows: Int = 200,
      bytes: Int = 32 * 1024,
      timeout: FiniteDuration = 10.seconds
  ): Limits =
    Limits.of(rows, bytes, timeout) match {
      case Right(l) => l
      case Left(why) => throw new IllegalStateException(why)
    }
}
