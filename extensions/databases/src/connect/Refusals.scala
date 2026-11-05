package grit.databases.connect

import grit.core.secret.{Secret, Variable}

/** Why a reader did not open. */
enum OpenRefusal {

  /** A variable unset or blank, or a secret's not named like one. */
  case Unread(why: Secret.Refusal)

  /** `url`'s value is not `jdbc:postgresql://[host[:port][,…]]/database[?parameters]`. */
  case NotPostgres(url: Variable)

  /** `url`'s value holds a user, a password or an `sslpassword`: the login says who. */
  case Credentialed(url: Variable)

  /** `url`'s value sets `key`, which grit sets itself (`logServerErrorDetail`,
    * `loginTimeout`, `socketTimeout`, `ApplicationName`, `maxResultBuffer`) or which would
    * undo a guard (`preferQueryMode`, `service`).
    */
  case Overrides(url: Variable, key: String)

  /** No JDBC driver on the classpath takes a Postgres URL: the deployment brings one. */
  case NoDriver

  case Unreachable(failure: Failure)

  /** Logged in as other than the role. */
  case WrongRole(seen: Seen)

  /** One line for the person starting the deployment, naming variables, never a secret's
    * value; an [[Unreachable]]'s says the driver's message, which may name a host or a user.
    */
  def message: String = this match {
    case Unread(why) => why.message
    case NotPostgres(v) =>
      s"${Variable.value(v)} does not hold a Postgres JDBC URL, jdbc:postgresql://[host[:port]]/database[?parameters]"
    case Credentialed(v) =>
      s"${Variable.value(v)} holds a URL with a credential in it (a user, a password or an sslpassword): the database's login gives them"
    case Overrides(v, key) =>
      s"${Variable.value(v)} holds a URL that sets $key, which grit sets or guards itself"
    case NoDriver =>
      "no JDBC driver for Postgres is on the classpath: the deployment depends on one, such as org.postgresql:postgresql"
    case Unreachable(f) => s"the database could not be opened: ${f.message}"
    case WrongRole(seen) =>
      s"the database logged in as ${seen.current} (session ${seen.session}), not as the role it declares"
  }
}

/** Why a query was not answered. */
enum QueryRefusal {

  /** The session is not the role, as after a reconnect that landed as another; the connection
    * is closed.
    */
  case RoleChanged(seen: Seen)

  /** It would write, or make the transaction writable (`25006`, `25001`). */
  case Writes

  /** It outran the limits' timeout (`57014`). */
  case TimedOut

  /** Its answer passed the driver's buffer, 16 times the limits' bytes and at least 4 MiB,
    * before the limits could clip it; the connection is closed.
    */
  case TooLarge

  /** The role may not read what it names (`42501`). */
  case NotPermitted(failure: Failure)

  /** The database refused it otherwise: its syntax, a name, a value. */
  case Rejected(failure: Failure)

  /** The connection failed or did not open again; the next query tries again. */
  case Unreachable(failure: Failure)
}

/** Who a session was: `current_user` and `session_user`. */
final case class Seen(current: String, session: String)

/** A database's or driver's failure: its SQLSTATE when it has one, and its message's first line
  * (a server's primary message, without its severity, its detail or its position).
  */
final case class Failure(state: Option[String], message: String)
