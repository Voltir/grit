package grit.mcp.client

import java.net.URI

import scala.util.Try

import grit.core.edge.Variable
import grit.mcp.scope.McpScope

/** An MCP server grit reaches over Streamable HTTP at [[grit.mcp.wire.Rpc.Version]]: its tools
  * are offered as `{name}_{tool}`, and of those only the ones `allow` names when it names any
  * and `scope` offers; its calls are sent, and their answers shown, as `scope` sends and shows
  * them; each request carries
  * `Authorization: Bearer` with the value of `token`.
  */
final case class McpServer private (
    name: String,
    endpoint: URI,
    token: Variable,
    scope: McpScope,
    allow: Set[String]
)

object McpServer {

  /** The server, or why not: `name` not a lowercase letter then lowercase letters, digits or
    * `_` (at most 16 characters); `endpoint` not an absolute `https` URL with a host (`http`
    * only on a loopback host: `localhost`, `127.x.x.x` or `[::1]`).
    */
  def of(
      name: String,
      endpoint: String,
      token: Variable,
      scope: McpScope,
      allow: Set[String] = Set.empty
  ): Either[String, McpServer] =
    for {
      _ <- Either.cond(
        Name.matches(name),
        (),
        s"an MCP server's name must be a lowercase letter then up to 15 lowercase letters, " +
          s"digits or _, not '$name'"
      )
      uri <- Try(new URI(endpoint)).toOption
        .filter(u => u.isAbsolute && Option(u.getHost).exists(_.nonEmpty))
        .toRight(s"$name's endpoint is not an absolute URL with a host: '$endpoint'")
      _ <- Option(uri.getScheme).map(_.toLowerCase) match {
        case Some("https") => Right(())
        case Some("http") if loopback(uri.getHost) => Right(())
        case _ =>
          Left(s"$name's endpoint must be https (http only on a loopback host): '$endpoint'")
      }
    } yield new McpServer(name, uri, token, scope, allow)

  private val Name = "[a-z][a-z0-9_]{0,15}".r

  private def loopback(host: String): Boolean =
    host == "localhost" || host == "[::1]" ||
      host.split('.').toVector.match {
        case Vector("127", rest*) =>
          rest.size == 3 && rest.forall(p => p.nonEmpty && p.forall(_.isDigit) && p.toInt <= 255)
        case _ => false
      }
}
