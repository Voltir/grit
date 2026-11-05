package grit.databases.connect

import grit.core.secret.Variable
import grit.databases.declare.Login

/** Whether a JDBC URL is one [[DatabaseReader.open]] takes. */
private[connect] object PostgresUrl {

  /** `url`, held in `variable`, as [[grit.databases.declare.Database]] says it must be, or why
    * not, naming `variable` alone.
    */
  def check(variable: Variable, url: String): Either[OpenRefusal, Unit] = {
    val prefix = "jdbc:postgresql://"
    val (server, query) = url.indexOf('?') match {
      case -1 => (url, "")
      case i => (url.substring(0, i), url.substring(i + 1))
    }
    val rest = if (server.startsWith(prefix)) Some(server.substring(prefix.length)) else None
    val formed = rest.exists { r =>
      val slash = r.indexOf('/')
      slash >= 0 && r.substring(slash + 1).nonEmpty && !r.substring(slash + 1).contains('/')
    }
    val keys = query.split('&').toVector.filter(_.nonEmpty).map(t => t.takeWhile(_ != '='))
    if (!formed) Left(OpenRefusal.NotPostgres(variable))
    else if (rest.exists(r => r.takeWhile(_ != '/').contains('@')))
      Left(OpenRefusal.Credentialed(variable))
    else if (keys.exists(k => Login.Credentials(k.toLowerCase) || k.equalsIgnoreCase("user")))
      Left(OpenRefusal.Credentialed(variable))
    else
      keys.find(k => Guarded(k.toLowerCase)) match {
        case Some(key) => Left(OpenRefusal.Overrides(variable, key))
        case None => Right(())
      }
  }

  /** The parameters a URL may not set, lowercased: those grit sets, which pgjdbc lets a URL's
    * parameter outrank, and those that would undo a guard (simple mode answers a result whole;
    * a pg_service.conf entry may hold a password).
    */
  private val Guarded: Set[String] = Set(
    "logservererrordetail",
    "logintimeout",
    "sockettimeout",
    "applicationname",
    "maxresultbuffer",
    "preferquerymode",
    "service"
  )
}
