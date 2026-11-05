package grit.databases.connect

import java.sql.SQLException

/** What reading a database says to Postgres, and how its failures read: the one place a second
  * engine would differ.
  */
private[connect] object Postgres {

  val Begin: String = "BEGIN READ ONLY"

  /** A statement timeout of `millis`, for the transaction alone. */
  def timeout(millis: Long): String = s"SET LOCAL statement_timeout = $millis"

  val Who: String = "SELECT current_user, session_user"

  val Rollback: String = "ROLLBACK"

  /** `e` as grit keeps it: its SQLSTATE, and its message's first line with a server's severity
    * (`ERROR: `, `FATAL: `) taken off.
    */
  def failure(e: SQLException): Failure = {
    val first = Option(e.getMessage).getOrElse("").linesIterator.nextOption().getOrElse("")
    Failure(Option(e.getSQLState), Severity.replaceFirstIn(first, ""))
  }

  private val Severity = "^(ERROR|FATAL|PANIC): ".r

  /** Whether `f` says the connection itself failed: SQLSTATE class 08, or the server shutting
    * the session down (`57P01` to `57P03`), or no SQLSTATE at all.
    */
  def broken(f: Failure): Boolean =
    f.state.forall(s => s.startsWith("08") || s == "57P01" || s == "57P02" || s == "57P03")

  /** Whether `f` is the driver's buffer for one answer running over. */
  def overflowed(f: Failure): Boolean = f.message.startsWith("Result set exceeded maxResultBuffer")
}
