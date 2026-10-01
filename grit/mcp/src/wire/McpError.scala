package grit.mcp.wire

/** Why one exchange with an MCP server came to nothing. None holds a credential. */
enum McpError {

  /** It could not be reached, timed out, or its stream ended before the response. */
  case Unreachable(why: String)

  /** HTTP 401: the token is missing or refused; `challenge`, its `WWW-Authenticate`. */
  case Unauthorized(challenge: Option[String])

  /** HTTP 403: the token lacks a permission; `challenge`, its `WWW-Authenticate`. */
  case Forbidden(challenge: Option[String])

  /** It does not take revision 2026-07-28; `supported`, the versions it named. */
  case Unsupported(supported: Vector[String])

  /** HTTP `status`, a 4xx, with no MCP error in the body: a server of an earlier revision. */
  case Legacy(status: Int)

  /** Any other JSON-RPC error: its `code`, and `said`, its message. */
  case Rpc(code: Int, said: String)

  /** The result wanted input from grit (`resultType: input_required`); grit gives none. */
  case InputRequired

  /** The answer was not a JSON-RPC response to the request, or its result not one the
    * method returns.
    */
  case Unreadable(why: String)

  /** Any other HTTP status. */
  case Status(code: Int)

  /** A line a person or the model reads, of the server unnamed ("refused grit's token"). */
  def message: String = this match {
    case Unreachable(why) => s"could not be reached: $why"
    case Unauthorized(challenge) => s"refused grit's token (HTTP 401${asks(challenge)})"
    case Forbidden(challenge) => s"grit's token lacks a permission (HTTP 403${asks(challenge)})"
    case Unsupported(supported) if supported.isEmpty =>
      "does not speak MCP 2026-07-28, and names no version it does"
    case Unsupported(supported) =>
      s"does not speak MCP 2026-07-28; it speaks ${supported.mkString(", ")}"
    case Legacy(status) =>
      s"answered HTTP $status with no MCP error: it speaks only an earlier revision of MCP"
    case Rpc(code, said) => s"answered error $code: $said"
    case InputRequired => "wanted input that grit does not give"
    case Unreadable(why) => s"answered something grit cannot read: $why"
    case Status(code) => s"answered HTTP $code"
  }

  private def asks(challenge: Option[String]): String = challenge.fold("")(c => s"; it asks: $c")
}
