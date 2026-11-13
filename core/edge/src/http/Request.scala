package grit.edge.http

/** A request's method; methods are matched with case. */
enum Verb {
  case Get, Head, Post, Put, Patch, Delete, Options

  /** Any other method, as sent. */
  case Other(sent: String)

  /** The method as a request line carries it. */
  def name: String = this match {
    case Other(sent) => sent
    case named => named.toString.toUpperCase
  }
}

object Verb {

  /** `name` as a named case when it is one exactly, else [[Verb.Other]]. */
  def of(name: String): Verb = Named.find(_.name == name).getOrElse(Other(name))

  private val Named: Vector[Verb] = Vector(Get, Head, Post, Put, Patch, Delete, Options)
}

/** A request as a [[Listener]] hands it to its answer: read whole, its body as text. */
final case class Request private (
    verb: Verb,
    path: Vector[String],
    body: String,
    private val sentQuery: Vector[(String, String)],
    private val sentHeaders: Vector[(String, String)]
) {

  /** The first value of the header `name`, matched without regard to case. */
  def header(name: String): Option[String] = headerValues(name).headOption

  /** Every value of the header `name`, matched without regard to case, in the order sent; empty
    * when none was.
    */
  def headerValues(name: String): Vector[String] =
    sentHeaders.collect { case (n, v) if n.equalsIgnoreCase(name) => v }

  /** The first value of the query parameter `name`, matched exactly. */
  def query(name: String): Option[String] = queryValues(name).headOption

  /** Every value of the query parameter `name`, matched exactly, in the order sent; empty when
    * none was.
    */
  def queryValues(name: String): Vector[String] =
    sentQuery.collect { case (n, v) if n == name => v }

  /** The name of every query parameter sent, each once, in the order first sent. */
  def queryNames: Vector[String] = sentQuery.map(_._1).distinct
}

object Request {

  /** `target` as a request line carries it (`/a/b%20c?x=1&y`), read with `verb`, `headers` and
    * `body`. The path is split at each `/` into segments, each percent-decoded: `/` is none, a
    * trailing `/` a last empty segment, `%2F` stays inside its segment, and `.` and `..` are
    * segments like any other, never resolved. The query is decoded as a form (`+` a space); a
    * name with no `=` has the value "". Left, saying what is wrong, when `target` does not start
    * with `/` (an absolute URL or `*`), or an escape is not `%` and two hex digits or does not
    * decode as UTF-8.
    */
  def of(
      verb: Verb,
      target: String,
      headers: Iterable[(String, String)],
      body: String
  ): Either[String, Request] =
    if (!target.startsWith("/")) Left("the target must be a path starting with /")
    else {
      val (rawPath, rawQuery) = target.indexOf('?') match {
        case -1 => (target, "")
        case at => (target.substring(0, at), target.substring(at + 1))
      }
      val segments: Vector[String] =
        if (rawPath == "/") Vector.empty else rawPath.substring(1).split("/", -1).toVector
      val pairs: Vector[(String, String)] =
        rawQuery.split("&").toVector.filter(_.nonEmpty).map { piece =>
          piece.indexOf('=') match {
            case -1 => (piece, "")
            case at => (piece.substring(0, at), piece.substring(at + 1))
          }
        }
      for {
        path <- every(segments)(decoded)
        names <- every(pairs.map(_._1))(n => decoded(n.replace('+', ' ')))
        values <- every(pairs.map(_._2))(v => decoded(v.replace('+', ' ')))
      } yield Request(verb, path, body, names.zip(values), headers.toVector)
    }

  /** `texts` each through `f`, or the first Left. */
  private def every(
      texts: Vector[String]
  )(f: String => Either[String, String]): Either[String, Vector[String]] = {
    val read: Vector[Either[String, String]] = texts.map(f)
    read.collectFirst { case Left(why) => why }.toLeft(read.collect { case Right(t) => t })
  }

  /** `text` with each `%` escape read as a byte, the bytes decoded as strict UTF-8. */
  private def decoded(text: String): Either[String, String] =
    if (!text.contains('%')) Right(text)
    else {
      val bytes = new java.io.ByteArrayOutputStream()
      var i = 0
      var bad = false
      while (i < text.length && !bad) {
        val c = text.charAt(i)
        if (c == '%') {
          val hex = if (i + 3 <= text.length) text.substring(i + 1, i + 3) else ""
          if (hex.length == 2 && hex.forall(h => Character.digit(h, 16) >= 0)) {
            bytes.write(Integer.parseInt(hex, 16))
            i += 3
          } else bad = true
        } else {
          bytes.writeBytes(c.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8))
          i += 1
        }
      }
      if (bad) Left("an escape must be % and two hex digits")
      else {
        val decoder = java.nio.charset.StandardCharsets.UTF_8
          .newDecoder()
          .onMalformedInput(java.nio.charset.CodingErrorAction.REPORT)
          .onUnmappableCharacter(java.nio.charset.CodingErrorAction.REPORT)
        scala.util
          .Try(decoder.decode(java.nio.ByteBuffer.wrap(bytes.toByteArray)).toString)
          .toOption
          .toRight("the escapes must decode as UTF-8")
      }
    }
}
