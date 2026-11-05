package grit.databases.connect

import java.sql.{Connection, DriverManager, SQLException, Statement}
import java.util.Properties

import grit.core.either.Traversal
import grit.core.secret.{Reveal, Secret, Variable}
import grit.databases.declare.{Database, DatabaseRole, Limits, Login}

/** A connection to a [[Database]] as its role alone, read-only. Its calls run one at a time. */
final class DatabaseReader private (
    role: DatabaseRole,
    limits: Limits,
    url: String,
    properties: Properties,
    first: Connection
) extends caps.SharedCapability
    with AutoCloseable {

  // The open connection, if any: read and replaced only inside this reader's synchronized
  // methods, under its monitor, and never handed out. Option[Connection] has an empty capture
  // set, so the annotation untracks nothing the checker tracks.
  @caps.unsafe.untrackedCaptures
  private var connection: Option[Connection] = Some(first)

  /** Ends the connection; a later call opens it again. */
  def close(): Unit = synchronized {
    connection.foreach(DatabaseReader.quietly)
    connection = None
  }
}

object DatabaseReader {

  /** `database` opened with `env`'s variables, its role checked in a first transaction; or why
    * not, never saying a secret's value. Waits at most `database.limits.timeout`, rounded up to
    * seconds, to log in.
    */
  def open(database: Database, env: Map[String, String]): Either[OpenRefusal, DatabaseReader] =
    opened(database, env) match {
      case Left(why) => Left(why)
      case Right((c, url, properties)) =>
        Right(new DatabaseReader(database.role, database.limits, url, properties, c))
    }

  /** A connection to `database` as its role, with the URL and properties it was opened with. */
  private def opened(
      database: Database,
      env: Map[String, String]
  ): Either[OpenRefusal, (Connection, String, Properties)] =
    for {
      url <- plain(env, database.url)
      _ <- PostgresUrl.check(database.url, url)
      properties <- login(database.login, env)
      _ <- driver(url)
      _ = grits(properties, database.limits)
      c <- connect(url, properties).left.map(OpenRefusal.Unreachable(_))
      seen <- checked(c, database.role, database.limits).left.map { f =>
        quietly(c)
        OpenRefusal.Unreachable(f)
      }
      _ <- seen.toLeft(()).left.map { s =>
        quietly(c)
        OpenRefusal.WrongRole(s)
      }
    } yield (c, url, properties)

  /** Who the session is, in a read-only transaction of its own under `limits`' timeout:
    * `None` when both `current_user` and `session_user` are `role`, else who it is.
    */
  private def checked(
      c: Connection,
      role: DatabaseRole,
      limits: Limits
  ): Either[Failure, Option[Seen]] =
    attempt {
      transaction(c, limits)(st => mismatch(who(st), role))
    }

  /** `body` run in a read-only transaction on `c` under `limits`' timeout, rolled back after. */
  private[connect] def transaction[A](c: Connection, limits: Limits)(body: Statement => A): A = {
    val st = c.createStatement()
    try {
      st.execute(Postgres.Begin)
      try {
        st.execute(Postgres.timeout(limits.timeout.toMillis))
        body(st)
      } finally st.execute(Postgres.Rollback)
    } finally st.close()
  }

  private[connect] def who(st: Statement): Seen = {
    val rs = st.executeQuery(Postgres.Who)
    try {
      if (rs.next()) Seen(rs.getString(1), rs.getString(2)) else Seen("", "")
    } finally rs.close()
  }

  /** `None` when `seen` is `role` twice, else `seen`. */
  private[connect] def mismatch(seen: Seen, role: DatabaseRole): Option[Seen] = {
    val name = DatabaseRole.name(role)
    if (seen.current == name && seen.session == name) None else Some(seen)
  }

  /** `body`, or the failure it threw. */
  private[connect] def attempt[A](body: => A): Either[Failure, A] =
    try Right(body)
    catch { case e: SQLException => Left(Postgres.failure(e)) }

  private[connect] def connect(url: String, properties: Properties): Either[Failure, Connection] =
    attempt {
      val c = DriverManager.getConnection(url, properties)
      c.setAutoCommit(true)
      c
    }

  private[connect] def quietly(c: Connection): Unit =
    try c.close()
    catch { case _: SQLException => () }

  /** A plain variable's value, trimmed, or why not. */
  private def plain(env: Map[String, String], variable: Variable): Either[OpenRefusal, String] =
    env.get(Variable.value(variable)) match {
      case None => Left(OpenRefusal.Unread(Secret.Refusal.Unset(variable)))
      case Some(v) if v.trim.isEmpty => Left(OpenRefusal.Unread(Secret.Refusal.Blank(variable)))
      case Some(v) => Right(v.trim)
    }

  private def secret(env: Map[String, String], variable: Variable): Either[OpenRefusal, Secret] =
    Secret.read(env, variable).left.map(OpenRefusal.Unread(_))

  /** The driver's properties `login` gives, its secrets revealed into them alone. */
  private def login(login: Login, env: Map[String, String]): Either[OpenRefusal, Properties] =
    login match {
      case Login.Password(user, password) =>
        for {
          u <- plain(env, user)
          s <- secret(env, password)
        } yield {
          val p = new Properties()
          p.setProperty("user", u)
          p.setProperty("password", Reveal.text(s))
          p
        }
      case d: Login.Driver =>
        Traversal.traverse(d.secrets.toVector)((key, v) => secret(env, v).map(key -> _)).map {
          secrets =>
            val p = new Properties()
            d.plain.foreach((k, v) => p.setProperty(k, v))
            secrets.foreach((k, s) => p.setProperty(k, Reveal.text(s)))
            p
        }
    }

  /** grit's own settings, set over whatever the login gave: no server detail (row values) in a
    * failure's message, the login and socket timeouts, its name to the server, and the driver's
    * buffer for one answer.
    */
  private def grits(p: Properties, limits: Limits): Unit = {
    val seconds = math.max(1L, (limits.timeout.toMillis + 999) / 1000)
    p.setProperty("logServerErrorDetail", "false")
    p.setProperty("loginTimeout", seconds.toString)
    p.setProperty("socketTimeout", (seconds + 5).toString)
    p.setProperty("ApplicationName", "grit")
    p.setProperty("maxResultBuffer", buffer(limits).toString)
  }

  /** The most bytes the driver reads of one answer: 16 times the limits' bytes, at least 4 MiB. */
  private[connect] def buffer(limits: Limits): Long = math.max(16L * limits.bytes, 4L * 1024 * 1024)

  private def driver(url: String): Either[OpenRefusal, Unit] =
    try {
      DriverManager.getDriver(url)
      Right(())
    } catch { case _: SQLException => Left(OpenRefusal.NoDriver) }
}
