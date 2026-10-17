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

  /** HTTP `status`, a 4xx, with no JSON-RPC error in the body; `body`, the body's start as
    * one line ([[Rpc.failure]]), empty when it had none. A server of an earlier revision
    * answers so, and so may any server refusing a request or a token it cannot read.
    */
  case Rejected(status: Int, body: String)

  /** Any other JSON-RPC error: its `code`, and `text`, its message. */
  case Rpc(code: Int, text: String)

  /** The result wanted input from grit (`resultType: input_required`); grit gives none. */
  case InputRequired

  /** The answer was not a JSON-RPC response to the request, or its result not one the
    * method returns.
    */
  case Unreadable(why: String)

  /** Any other HTTP status. */
  case Status(code: Int)

  /** The call was not sent: the server's scope does not offer its tool, or admits no call with
    * its arguments; `why` says which, naming the bounds and the call's values of their
    * arguments.
    */
  case OutOfScope(why: String)

  /** The call was answered, but its results cannot be held to the server's scope, so none of it
    * is shown: `why`.
    */
  case Unattributed(why: String)

  /** The call was answered, but its server's extension could not tell who may read the answer,
    * so none of it is shown: `why`, in words the model reads
    * ([[grit.core.visibility.Itemised.Unplaced]]).
    */
  case Unplaced(why: String)

  /** No token could be got for the request, so it was not sent: `why`, naming the token URL
    * and what it answered, if anything.
    */
  case NoToken(why: String)

  /** The call was not sent: a lookup it needs first (`grit.mcp.client.McpServer.lookups`)
    * could not be made; `why` says which, and why.
    */
  case NotLooked(why: String)

  /** A line a person or the model reads, of the server unnamed ("refused the token it was given"). */
  def message: String = this match {
    case Unreachable(why) => s"could not be reached: $why"
    case Unauthorized(challenge) => s"refused the token it was given (HTTP 401${asks(challenge)})"
    case Forbidden(challenge) =>
      s"refused the call, since its token lacks a permission (HTTP 403${asks(challenge)})"
    case Unsupported(supported) if supported.isEmpty =>
      "does not speak MCP 2026-07-28, and names no version it does"
    case Unsupported(supported) =>
      s"does not speak MCP 2026-07-28; it speaks ${supported.mkString(", ")}"
    case Rejected(status, body) if body.isEmpty => s"answered HTTP $status with no MCP error"
    case Rejected(status, body) => s"answered HTTP $status with no MCP error: $body"
    case Rpc(code, text) => s"answered error $code: $text"
    case InputRequired => "asked for input partway through the call, which cannot be given"
    case Unreadable(why) => s"answered something that could not be read: $why"
    case Status(code) => s"answered HTTP $code"
    case OutOfScope(why) =>
      s"was not asked, so nothing of it was read: $why. Tell the person this is out of " +
        "reach; do not answer with something else in its place."
    case Unattributed(why) =>
      s"answered, but its answer could not be checked as within what this server may read, so none of it is shown: $why"
    case Unplaced(why) =>
      s"answered, but who may see the answer could not be told, so none of it is shown: $why"
    case NoToken(why) => s"was not asked: no token could be got for it: $why"
    case NotLooked(why) =>
      s"was not asked, since what it would read could not be looked up first: $why"
  }

  /** What the operator's log is told of it: its [[message]], the server's own words in a
    * [[Rejected]] or an [[Rpc]] as one line of at most [[McpError.LoggedMost]] characters, cut
    * at a word and ending " [cut]" when longer, since a server may echo a long argument or
    * answer; `None` for [[OutOfScope]], whose words name the call's arguments, and which the
    * model's own call explains.
    */
  def logged: Option[String] = this match {
    case OutOfScope(_) => None
    case Rejected(status, body) => Some(Rejected(status, McpError.capped(body)).message)
    case Rpc(code, text) => Some(Rpc(code, McpError.capped(text)).message)
    case other => Some(other.message)
  }

  private def asks(challenge: Option[String]): String = challenge.fold("")(c => s"; it asks: $c")
}

object McpError {

  /** The most characters of a server's own words the operator's log is told: 200. */
  val LoggedMost: Int = 200

  /** `said` as one line, cut to [[LoggedMost]] characters at a word, as [[McpError.logged]]
    * says.
    */
  private def capped(said: String): String = {
    val line = said.trim.split("\\s+").mkString(" ")
    if (line.length <= LoggedMost) line
    else {
      val cut = line.take(LoggedMost)
      val word = cut.lastIndexOf(' ')
      s"${if (word > 0) cut.take(word) else cut} [cut]"
    }
  }
}
