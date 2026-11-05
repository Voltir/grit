package grit.databases.connect

import java.nio.charset.StandardCharsets
import java.sql.{Connection, DriverManager, ResultSet, SQLException, Statement}
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

  /** `statement`'s rows, `None` when it returns none at all (a SET), run in a transaction of its
    * own that is read-only, under the limits' timeout, and rolled back, so nothing it sets
    * reaches the next. First, in that transaction, `current_user` and `session_user` must both
    * be the role. A connection found closed is opened again first. At most the limits' rows and
    * bytes, the clip said. Or why not.
    */
  def query(statement: OneStatement): Either[QueryRefusal, Option[Rows]] = synchronized {
    session { st =>
      st.setMaxRows(limits.rows + 1)
      if (st.execute(OneStatement.text(statement))) {
        val rs = st.getResultSet
        try Some(DatabaseReader.rows(rs, limits))
        finally rs.close()
      } else None
    }
  }

  /** Ends the connection; a later call opens it again. */
  def close(): Unit = synchronized {
    drop()
  }

  /** `body` on a live connection, in a read-only transaction of its own under the limits'
    * timeout, after the role check; the connection dropped when the session is not the role or
    * the failure says it broke.
    */
  private def session[A](body: Statement => A): Either[QueryRefusal, A] =
    live() match {
      case Left(f) => Left(QueryRefusal.Unreachable(f))
      case Right(c) =>
        val ran = DatabaseReader.attempt {
          DatabaseReader.transaction(c, limits) { st =>
            DatabaseReader.mismatch(DatabaseReader.who(st), role) match {
              case Some(seen) => Left(seen)
              case None => Right(body(st))
            }
          }
        }
        ran match {
          case Right(Right(a)) => Right(a)
          case Right(Left(seen)) =>
            drop()
            Left(QueryRefusal.RoleChanged(seen))
          case Left(f) => Left(refusal(f))
        }
    }

  /** What `f` says of a statement, the connection dropped when it says that broke. */
  private def refusal(f: Failure): QueryRefusal = f.state match {
    case Some("25006") | Some("25001") => QueryRefusal.Writes
    case Some("57014") => QueryRefusal.TimedOut
    case Some("42501") => QueryRefusal.NotPermitted(f)
    case _ if Postgres.overflowed(f) =>
      drop()
      QueryRefusal.TooLarge
    case _ if Postgres.broken(f) =>
      drop()
      QueryRefusal.Unreachable(f)
    case _ => QueryRefusal.Rejected(f)
  }

  /** The connection, opened again when there is none or it no longer answers. */
  private def live(): Either[Failure, Connection] =
    connection match {
      case Some(c) if DatabaseReader.valid(c) => Right(c)
      case _ =>
        drop()
        DatabaseReader.connect(url, properties).map { c =>
          connection = Some(c)
          c
        }
    }

  private def drop(): Unit = {
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
      val a =
        try {
          st.execute(Postgres.timeout(limits.timeout.toMillis))
          body(st)
        } catch {
          case e: SQLException =>
            // The failure is the body's; a rollback that fails too says only that it broke.
            try st.execute(Postgres.Rollback)
            catch { case _: SQLException => () }
            throw e
        }
      st.execute(Postgres.Rollback)
      a
    } finally st.close()
  }

  /** `rs`'s rows as `limits` keep them: whole rows while they and the columns' names fit the
    * limits' bytes of UTF-8, at most the limits' rows, the clip said.
    */
  private[connect] def rows(rs: ResultSet, limits: Limits): Rows = {
    val meta = rs.getMetaData
    val width = meta.getColumnCount
    val columns = Vector.tabulate(width)(i => meta.getColumnLabel(i + 1))
    var kept = Vector.empty[Vector[Option[String]]]
    var bytes = columns.map(utf8).sum
    var clip: Option[Clip] = None
    while (clip.isEmpty && rs.next()) {
      if (kept.size == limits.rows) clip = Some(Clip.Rows)
      else {
        val row = Vector.tabulate(width)(i => Option(rs.getString(i + 1)))
        val size = row.map(_.fold(0)(utf8)).sum
        if (bytes + size > limits.bytes) clip = Some(Clip.Bytes)
        else {
          kept = kept :+ row
          bytes += size
        }
      }
    }
    Rows(columns, kept, clip)
  }

  private def utf8(s: String): Int = s.getBytes(StandardCharsets.UTF_8).length

  /** Whether `c` still answers, within a second. */
  private[connect] def valid(c: Connection): Boolean =
    try c.isValid(1)
    catch { case _: SQLException => false }

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
