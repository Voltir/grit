package grit.mcp.wire

/** The JSON-RPC error codes a server here writes and grit's client reads by number. */
private[wire] object Code {
  val ParseError: Int = -32700
  val InvalidRequest: Int = -32600
  val MethodNotFound: Int = -32601
  val InvalidParams: Int = -32602
  val HeaderMismatch: Int = -32020
  val UnsupportedVersion: Int = -32022
}
