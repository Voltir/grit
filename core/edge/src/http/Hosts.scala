package grit.edge.http

import java.util.Locale

/** The names a request's `Host` may give a listener, at any port: what keeps a page another site
  * serves from reaching a route, through DNS rebinding (which sends the rebound name as `Host`)
  * or a cross-site request (which sends its own `Origin` or `Sec-Fetch-Site`).
  */
final case class Hosts private (names: Set[String]) {

  /** Whether `request` is admitted: its `Host` names one of these, in any case, at any port or
    * none, an IPv6 literal in brackets (`[::1]:7317`); its `Origin`, when sent, is `http://`
    * that same `Host`, compared without regard to case, so `Origin: null`, `https://` and
    * another port are refused; and its `Sec-Fetch-Site`, when sent, is `same-origin` or `none`.
    * A request with no `Host` is refused. Nothing here tells a write from a read: a route that a
    * GET reaches must write nothing, since a page may send a GET with neither header.
    */
  def admit(request: Request): Boolean =
    request.header("Host").exists { host =>
      Hosts.named(host).exists(names.contains) &&
      request.header("Origin").forall(_.equalsIgnoreCase(s"http://$host")) &&
      request
        .header("Sec-Fetch-Site")
        .forall(s => Hosts.SameSite.contains(s.toLowerCase(Locale.ROOT)))
    }
}

object Hosts {

  /** `localhost`, `127.0.0.1` and `::1`: the names a listener on loopback is reached by. */
  val loopback: Hosts = new Hosts(Set("localhost", "127.0.0.1", "::1"))

  /** `names`, each a host name of `.`-separated labels of ASCII letters, digits and `-`, each
    * starting and ending with a letter or digit, or an IPv6 literal of ASCII hex digits, `:` and
    * `.` without brackets, compared without regard to case: the names a listener bound to every address
    * declares it is reached by. `Left` saying why when `names` is empty, or naming the first
    * that is no host's name.
    */
  def of(names: Vector[String]): Either[String, Hosts] =
    if (names.isEmpty) Left("a listener's hosts name one at least")
    else
      names.find(n => !written(n)) match {
        case Some(bad) => Left(s"\"$bad\" is no host's name")
        case None => Right(new Hosts(names.map(_.toLowerCase(Locale.ROOT)).toSet))
      }

  private val SameSite = Set("same-origin", "none")

  private def written(name: String): Boolean = {
    def alnum(c: Char) = c < 128 && c.isLetterOrDigit
    def hex(c: Char) = c < 128 && Character.digit(c, 16) >= 0
    def label(l: String) =
      l.nonEmpty && alnum(l.head) && alnum(l.last) && l.forall(c => alnum(c) || c == '-')
    name.split("\\.", -1).forall(label) ||
    name.contains(':') && name.forall(c => hex(c) || c == ':' || c == '.')
  }

  /** The name a `Host` header gives, lower-cased, without its port or an IPv6 literal's
    * brackets; `None` when it is not written as `name`, `name:port`, `[v6]` or `[v6]:port`.
    */
  private def named(host: String): Option[String] = {
    val (name, rest) =
      if (host.startsWith("[")) {
        val close = host.indexOf(']')
        if (close < 0) ("", "") else (host.substring(1, close), host.substring(close + 1))
      } else
        host.indexOf(':') match {
          case -1 => (host, "")
          case i => (host.substring(0, i), host.substring(i))
        }
    val port = rest.isEmpty || rest.length > 1 && rest.head == ':' && rest.tail.forall(_.isDigit)
    Option.when(name.nonEmpty && port)(name.toLowerCase(Locale.ROOT))
  }
}
