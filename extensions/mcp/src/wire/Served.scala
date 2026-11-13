package grit.mcp.wire

import grit.edge.http.Response

/** What a server says of itself at `server/discover` and `initialize`: its `name` and
  * `version`, and `instructions` for the model, if any.
  */
final case class ServerInfo(name: String, version: String, instructions: Option[String])

/** What a POST to an MCP endpoint, read by [[Rpc.received]], leaves its server to do. */
sealed trait Asked

object Asked {

  /** Nothing more: send `response`. */
  final case class Answered(response: Response) extends Asked

  /** `tools/list` from `cursor`, `None` for the first page. */
  final case class List private[wire] (
      cursor: Option[String],
      private[wire] val id: ujson.Value,
      private[wire] val revision: Revision
  ) extends Asked

  /** `tools/call` of `tool`, as the client named it, with `arguments`, `{}` when it sent none. */
  final case class Call private[wire] (
      tool: String,
      arguments: ujson.Obj,
      private[wire] val id: ujson.Value,
      private[wire] val revision: Revision
  ) extends Asked
}
