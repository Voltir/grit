package grit.dbos.internal

import java.sql.DriverManager

import scala.util.Using
import scala.util.control.NonFatal

import grit.dbos.sql.DbConfig

/** A database made empty for one run, beside another on the same server, and dropped only by
  * whoever made it: an eval case drawn apart from every other case, or an integration suite's.
  */
trait Scratch extends caps.SharedCapability {

  /** It, reached with the login of the database it was made beside. */
  val config: DbConfig

  /** Drops it; why not, when a session is still open on it or the server refuses. */
  def drop(): Either[String, Unit]
}

object Scratch {

  /** A new, empty database `name` on the server `beside` names, made over a session on `beside`'s
    * own database; why not: `name` is not a lowercase letter then lowercase letters, digits and
    * `_`, the database exists already, or the server refuses.
    */
  def create(beside: DbConfig, name: String): Either[String, Scratch^] =
    if (!Named.matches(name)) Left(s"$name is not a database name grit makes")
    else
      run(beside, s"CREATE DATABASE $name") match {
        case Left(why) => Left(why)
        case Right(()) =>
          val params = beside.jdbcUrl.dropWhile(_ != '?')
          val config = DbConfig(
            s"jdbc:postgresql://${beside.endpoint.hosts}/$name$params",
            beside.user,
            beside.password
          )
          Right(new Made(beside, name, config))
      }

  private val Named = "[a-z][a-z0-9_]*".r

  private final class Made(beside: DbConfig, name: String, val config: DbConfig) extends Scratch {
    def drop(): Either[String, Unit] = run(beside, s"DROP DATABASE $name")
  }

  /** `sql` over one session on `beside`'s database, or why not: the server's own words. */
  private def run(beside: DbConfig, sql: String): Either[String, Unit] =
    try {
      Using.resource(DriverManager.getConnection(beside.jdbcUrl, beside.user, beside.password)) {
        conn => Using.resource(conn.createStatement())(_.execute(sql))
      }
      Right(())
    } catch {
      case NonFatal(e) => Left(s"$sql: ${e.getMessage}")
    }
}
