package grit.edge.http

/** An answer's status: the ones grit's edges answer with. */
enum Status(val code: Int) {
  case Ok extends Status(200)
  case Accepted extends Status(202)
  case BadRequest extends Status(400)

  /** 401, sent with a `WWW-Authenticate` header holding `challenge`. */
  case Unauthorized(challenge: Challenge) extends Status(401)
  case Forbidden extends Status(403)
  case NotFound extends Status(404)

  /** 405, sent with an `Allow` header naming `allowed`. */
  case NotAllowed(allowed: Vector[Verb]) extends Status(405)
  case Timeout extends Status(408)
  case Conflict extends Status(409)
  case TooLarge extends Status(413)
  case Failed extends Status(500)
  case Unavailable extends Status(503)
}

/** What a 401 asks for: a bearer credential (RFC 6750), naming no authorization server.
  * `Asked`: none usable was sent (`WWW-Authenticate: Bearer`). `Refused`: the one sent is not
  * taken (`Bearer error="invalid_token"`).
  */
enum Challenge {
  case Asked, Refused

  /** This 401, its body one line saying what to send. */
  def refused: Response = Response.text(
    Status.Unauthorized(this),
    this match {
      case Asked =>
        "refused: this edge needs a credential: send Authorization: Bearer <your credential>"
      case Refused => "refused: the credential sent is not one this edge knows"
    }
  )
}

/** What a response's body is. */
enum ContentType(val header: String) {
  case Text extends ContentType("text/plain; charset=utf-8")
  case Json extends ContentType("application/json")
}

/** What a request is answered with; a body of "" is sent as none. */
final case class Response private (status: Status, body: String, contentType: ContentType)

object Response {

  /** `body` as plain text. */
  def text(status: Status, body: String): Response = Response(status, body, ContentType.Text)

  /** `body` written as JSON. */
  def json(status: Status, body: ujson.Value): Response =
    Response(status, ujson.write(body), ContentType.Json)
}
