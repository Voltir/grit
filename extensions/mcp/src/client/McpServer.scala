package grit.mcp.client

import java.net.URI

import scala.util.Try

import grit.core.visibility.Accounts
import grit.mcp.scope.McpScope
import grit.mcp.wire.McpCalled

/** An MCP server grit reaches over Streamable HTTP at [[grit.mcp.wire.Rpc.Version]]: its tools
  * are offered as `{name}_{tool}`, and of those only the ones `allow` names when it names any
  * and `scope` offers; its calls are sent, and their answers shown, as `scope` sends and shows
  * them, and their items read by `accounts`; each request carries `Authorization: Bearer` with
  * the token `credential` gives. Before a call of a tool `lookups` names, each of its lookups
  * is called, in order, and its answer, as the server gave it, read by `accounts` beside the
  * call's ([[grit.mcp.wire.McpCalled.looked]]); a call lacking a lookup's argument, or one
  * whose lookup the server refuses or does not list as read-only, is not sent
  * ([[grit.mcp.wire.McpError.NotLooked]]). A lookup is grit's own call, made whatever `allow`
  * and `scope` say of its tool, with the one argument the call already sends.
  */
final case class McpServer private (
    name: String,
    endpoint: URI,
    credential: Credential,
    scope: McpScope,
    allow: Set[String],
    accounts: Accounts[McpCalled],
    lookups: Map[String, Vector[Lookup]]
)

/** A call made before a call of a tool that names its subject by `argument` but not where it
  * lies: `tool`, by the server's own name, called with `{argument: <the call's value>}`.
  */
final case class Lookup private (tool: String, argument: String)

object Lookup {

  /** The lookup, or why not: either blank. */
  def of(tool: String, argument: String): Either[String, Lookup] =
    if (tool.trim.isEmpty) Left("a lookup names no tool")
    else if (argument.trim.isEmpty) Left(s"a lookup of $tool names no argument")
    else Right(Lookup(tool, argument))
}

object McpServer {

  /** The server, or why not: `name` not a lowercase letter then lowercase letters, digits or
    * `_` (at most 16 characters); `endpoint` not an absolute `https` URL with a host (`http`
    * only on a loopback host: `localhost`, `127.x.x.x` or `[::1]`); a tool in `lookups`
    * looking itself up, or one tool twice.
    */
  def of(
      name: String,
      endpoint: String,
      credential: Credential,
      scope: McpScope,
      allow: Set[String] = Set.empty,
      accounts: Accounts[McpCalled] = Accounts.none,
      lookups: Map[String, Vector[Lookup]] = Map.empty
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
      _ <- Either.cond(
        secure(uri),
        (),
        s"$name's endpoint must be https (http only on a loopback host): '$endpoint'"
      )
      _ <- lookups.toVector.sortBy(_._1).foldLeft[Either[String, Unit]](Right(())) {
        case (read, (tool, ls)) =>
          val tools = ls.map(_.tool)
          read.flatMap { _ =>
            if (tools.contains(tool)) Left(s"$name's $tool looks itself up")
            else
              tools
                .diff(tools.distinct)
                .headOption
                .fold(Right(()))(t => Left(s"$name's $tool looks up $t twice"))
          }
      }
    } yield new McpServer(name, uri, credential, scope, allow, accounts, lookups)

  private val Name = "[a-z][a-z0-9_]{0,15}".r

  /** Whether `uri` is absolute with a host, and `https`, or `http` on a loopback host. */
  private[client] def secure(uri: URI): Boolean =
    uri.isAbsolute && Option(uri.getHost).exists(_.nonEmpty) &&
      (Option(uri.getScheme).map(_.toLowerCase) match {
        case Some("https") => true
        case Some("http") => loopback(uri.getHost)
        case _ => false
      })

  private def loopback(host: String): Boolean =
    host == "localhost" || host == "[::1]" ||
      host.split('.').toVector.match {
        case Vector("127", rest*) =>
          rest.size == 3 && rest.forall(p => p.nonEmpty && p.forall(_.isDigit) && p.toInt <= 255)
        case _ => false
      }
}
