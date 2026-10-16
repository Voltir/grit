package grit.mcp.wire

/** A `tools/call` exchange as its server's scope sends and shows it: `tool`, by the server's own
  * name, called with `arguments`, answered `result` before it is cut to fit; `looked`, the
  * answer of each lookup made before it ([[grit.mcp.client.McpServer.lookups]]) by its tool's
  * name, as the server gave it, an `isError` one included. What a server's
  * [[grit.core.visibility.Accounts]] reads.
  */
final case class McpCalled(
    tool: String,
    arguments: ujson.Obj,
    result: ujson.Obj,
    looked: Map[String, ujson.Obj] = Map.empty
)
