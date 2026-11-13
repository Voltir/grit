package grit.edge.http

/** An answer's status: the ones grit's edges answer with. */
enum Status(val code: Int) {
  case Ok extends Status(200)
  case Accepted extends Status(202)
  case BadRequest extends Status(400)
  case Forbidden extends Status(403)
  case NotFound extends Status(404)

  /** 405, sent with an `Allow` header naming `allowed`. */
  case NotAllowed(allowed: Vector[Verb]) extends Status(405)
  case TooLarge extends Status(413)
  case Failed extends Status(500)
  case Unavailable extends Status(503)
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
