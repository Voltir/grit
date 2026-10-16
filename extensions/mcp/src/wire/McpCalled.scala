package grit.mcp.wire

/** A `tools/call` exchange as its server's scope sends and shows it: `tool`, by the server's own
  * name, called with `arguments`, answered `result` before it is cut to fit. What a server's
  * [[grit.core.visibility.Accounts]] reads.
  */
final case class McpCalled(tool: String, arguments: ujson.Obj, result: ujson.Obj)
