package grit.mcp.client

import grit.mcp.wire.{McpError, Rpc}

/** How a client's requests reach its server, one transport's way ([[McpTransport]]). */
private[client] trait Connection {

  /** The result of one request making `call`, or why none. */
  def exchange(call: Rpc.Call): Either[McpError, ujson.Obj]

  /** Ends the transport; a request after it is [[McpError.Unreachable]]. */
  def close(): Unit
}
