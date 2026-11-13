package grit.edge.http

/** Where a [[Listener]] listens: a host, an IP literal or a name, and a port. */
final case class Listen private (host: String, port: Int) {

  /** Whether only this machine can reach it: a host in 127.0.0.0/8, `::1` however written, or the
    * name `localhost` in any case. Any other host, a wildcard (`0.0.0.0`, `::`) or an IPv4-mapped
    * address among them, is not.
    */
  def loopback: Boolean =
    host.equalsIgnoreCase("localhost") ||
      Listen.ipv4(host).exists(_.headOption.contains(127)) ||
      Listen.ipv6(host).contains(Vector.fill(7)(0) :+ 1)

  /** `host:port` as [[Listen.parse]] reads it, an IPv6 host in brackets. */
  def shown: String = if (host.contains(':')) s"[$host]:$port" else s"$host:$port"

  /** This address with the port a listener was given for it. */
  private[http] def bound(actual: Int): Listen = copy(port = actual)
}

object Listen {

  /** `text` as `host:port`: an IPv4 literal, an IPv6 literal in brackets (`[::1]:7317`), or a name
    * of letters, digits, `-` and `.`; a port from 0 to 65535, 0 for any free one. Left, saying what
    * it must be, when it is not, fit for [[grit.core.edge.EdgeRefusal.Malformed]]. A name is
    * resolved only by [[Listener.open]].
    */
  def parse(text: String): Either[String, Listen] =
    if (text.startsWith("[")) {
      text.indexOf("]:") match {
        case -1 => Left("an IPv6 host must be in brackets and followed by :port, as [::1]:7317")
        case end =>
          val host = text.substring(1, end)
          if (ipv6(host).isEmpty) Left(s"[$host] must be an IPv6 literal, as [::1]")
          else portOf(text.substring(end + 2)).map(Listen(host, _))
      }
    } else {
      text.lastIndexOf(':') match {
        case -1 => Left(s"the address must be $Shape")
        case at =>
          val host = text.substring(0, at)
          if (host.contains(':')) Left("an IPv6 host must be in brackets, as [::1]:7317")
          else for { h <- hostOf(host); p <- portOf(text.substring(at + 1)) } yield Listen(h, p)
      }
    }

  private val Shape = "host:port, as 127.0.0.1:7317, [::1]:7317 or localhost:7317"

  private def hostOf(host: String): Either[String, String] =
    if (host.isEmpty) Left(s"the host is missing: the address must be $Shape")
    else if (host.forall(c => ascii(c, '0', '9') || c == '.'))
      ipv4(host).map(_ => host).toRight(s"$host must be an IPv4 literal, as 127.0.0.1")
    else if (
      host.forall(c =>
        ascii(c, 'a', 'z') || ascii(c, 'A', 'Z') || ascii(c, '0', '9') || c == '-' || c == '.'
      )
    )
      Right(host)
    else Left(s"$host must be a name of letters, digits, - and ., or an IP literal")

  private def portOf(text: String): Either[String, Int] =
    if (text.isEmpty) Left(s"the port is missing: the address must be $Shape")
    else if (text.length <= 5 && text.forall(ascii(_, '0', '9')) && text.toInt <= 65535)
      Right(text.toInt)
    else Left(s"the port must be a number from 0 to 65535, not $text")

  private def ascii(c: Char, from: Char, to: Char): Boolean = c >= from && c <= to

  /** `host`'s four octets when it is an IPv4 literal, each written without a leading zero. */
  private def ipv4(host: String): Option[Vector[Int]] = {
    val parts = host.split("\\.", -1).toVector
    val octets = parts.collect {
      case p
          if p.nonEmpty && p.length <= 3 && p
            .forall(ascii(_, '0', '9')) && (p == "0" || !p.startsWith("0")) &&
            p.toInt <= 255 =>
        p.toInt
    }
    Option.when(parts.size == 4 && octets.size == 4)(octets)
  }

  /** `host`'s eight groups when it is an IPv6 literal: hex groups, at most one `::`, and an IPv4
    * literal as its last two groups when it ends with one.
    */
  private def ipv6(host: String): Option[Vector[Int]] = {
    def hex(g: String): Option[Int] =
      Option.when(
        g.nonEmpty && g.length <= 4 && g.forall(c =>
          ascii(c, '0', '9') || ascii(c, 'a', 'f') || ascii(c, 'A', 'F')
        )
      )(Integer.parseInt(g, 16))
    def groups(part: String, last: Boolean): Option[Vector[Int]] =
      if (part.isEmpty) Some(Vector.empty)
      else {
        val raw = part.split(":", -1).toVector
        val tail: Option[Vector[Int]] = raw.lastOption.flatMap { g =>
          if (last && g.contains('.')) ipv4(g).collect { case Vector(a, b, c, d) =>
            Vector(a * 256 + b, c * 256 + d)
          }
          else hex(g).map(Vector(_))
        }
        val init = raw.dropRight(1).map(hex)
        tail.filter(_ => init.forall(_.isDefined)).map((t: Vector[Int]) => init.flatten ++ t)
      }
    host.indexOf("::") match {
      case -1 => groups(host, last = true).filter(_.size == 8)
      case at if host.indexOf("::", at + 1) == -1 =>
        for {
          left <- groups(host.substring(0, at), last = false)
          right <- groups(host.substring(at + 2), last = true)
          if left.size + right.size <= 7
        } yield left ++ Vector.fill(8 - left.size - right.size)(0) ++ right
      case _ => None
    }
  }
}
